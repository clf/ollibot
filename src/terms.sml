

functor Terms
(
(* Internalized terms are indexes into arrays *)
type ndx
val void : ndx
datatype cell = Ndx of ndx | BaseConst of int * int | Void
datatype int_term = 
    ILam of int * cell
  | IConst of int * cell list
  | IVar of ndx * cell list

structure A : sig
  type growarray
  val init : int -> growarray
  val length : growarray -> ndx
  val append : growarray -> int_term -> unit
  val sub    : growarray -> ndx -> int_term
  val update : growarray -> ndx -> int_term -> unit
end

(* Closed reduced terms are keys to the hashtable *)
datatype red_term =
    RLam of int * red_term
  | RConst of int * red_term list
  | RVar  of ndx * red_term list 
  | RFVar of int * red_term list
  | RCell of cell

structure HT : sig
  type 'a hash_table
  type key
  val mkTable : int * exn -> 'a hash_table
  val clear   : 'a hash_table -> unit
  val insert  : 'a hash_table -> key * 'a -> unit
  val lookup  : 'a hash_table -> key -> 'a
  val find    : 'a hash_table -> key -> 'a option
end

(* Key takes any closed term that is not immediately a cell and turns it
 * into a hashtable key. *)
val key : red_term -> HT.key
) = struct

  open Lambda
  exception Invariant

  structure HC = HashCons
  structure HS = HashConsString

  datatype table = 
      T of {term_table : ndx HT.hash_table,
            term_array : A.growarray,
            cid_to_const : cid -> int,
            const_to_cid : int -> cid,
            construct_1 : int -> (int * int) option,
            construct_2 : int * (int * int) -> (int * int) option,
            sko_const   : int -> cid,
            typ_to_const : typ -> int}

  val strip_to_cell = map (fn RCell c => c | _ => raise Invariant)

  (* Internalize a closed application, checking for the possibility 
   * of base constants. *)
  fun intern_app (t as T{term_table, term_array, construct_1, construct_2,...},
                  cid, tms) = 
      let 
        val cells = strip_to_cell tms
        val len = length cells
        val baseconst = 
            case cells of 
              [] => construct_1 cid
            | [BaseConst(ty,v)] => construct_2 (cid,(ty,v)) 
            | _ => NONE
      in
        case baseconst of
          SOME const => BaseConst const
        | NONE =>
          let 
            val key = key (RConst(cid, tms))
          in
            case HT.find term_table key of
              SOME ndx => Ndx ndx
            | NONE =>
              let
                val next = A.length term_array
              in
                A.append term_array (IConst(cid,cells));
                HT.insert term_table (key, next);
                Ndx next
              end
          end
      end

  (* Internalize a closed Skolem pointer *)
  fun intern_var (t as T{term_table, term_array,...}, ndx, tms) = 
      let
        val key = key (RVar(ndx, tms))
        val cells = strip_to_cell tms
      in
        case HT.find term_table key of
          SOME ndx => Ndx ndx
        | NONE =>
          let 
            val next = A.length term_array
          in
            A.append term_array (IVar(ndx, cells));
            HT.insert term_table (key, next);
            Ndx next
          end
      end

  (* Internalize a closed lambda *)
  fun intern_lam (t as T{term_table, term_array,...}, ty, tm) = 
      let 
        val key = key(RLam(ty,tm))
      in
        case HT.find term_table key of
          SOME ndx => Ndx ndx
        | NONE => 
          let 
            val next = A.length term_array
          in
            A.append term_array (ILam(ty, Void));
            HT.insert term_table (key, next);

            (* We know that this expression has one free variable. The
             * next round of internalization substitutes for this free 
             * variable, and so we can expect that it will fully internalize.
             * We can then update our lambda with knowledge of its body. *)
            case intern(t, tm, 1, next)  of
              (RCell cell,0) => A.update term_array next (ILam(ty, cell))
            | _ => raise Invariant;

            Ndx next
          end
      end

  (* intern(t,tm,i,ndx) = (rtm, free)
   * 
   * Takes table info and a term and returns a reduced term and an integer
   * describing the "deepest" free variable. Maintains the invariant that
   * if free = 0, then rtm = RCell(cell). 
   * 
   * Also attempts to substitute the Skolem pointer ndx for the bound variable
   * i if possible. If the function is being used to internalize a closed
   * term, just call with i = 1 and ndx = void. 
   *)
  and intern (t as T{...}, tm, i, ndx)  = 
      let 
        fun intern_chain (tm, (rtms, free)) = 
            let val (rtm, free') = intern (t, tm, i, ndx) in
              (rtm :: rtms, Int.max(free, free'))
            end
        fun args (TBase cid) = []
          | args (TArrow(t1,t2)) = t1 :: args t2
      in
        case tm of
          RLam(ty,tm) => 
          let val (rtm, free) = intern(t, tm, i+1, ndx) in
            if free <= 1 
            then (RCell(intern_lam(t, ty, tm)), 0)
            else (RLam(ty, rtm), free - 1)
          end
        | RConst(cid, tms) => 
          let val (rtms, free) = foldl intern_chain ([], 0) tms in
            if free = 0
            then (RCell(intern_app(t,cid,rtms)), 0)
            else (RConst(cid, rtms), free)
          end
        | RVar(ndx', tms) =>
          let val (rtms, free) = foldl intern_chain ([], 0) tms in
            if free = 0
            then (RCell(intern_var(t,ndx',rtms)), 0)
            else (RVar(ndx', rtms), free)
          end
        | RFVar(j, tms) =>
          let val (rtms, free) = foldl intern_chain ([], 0) tms in
            if i = j andalso free = 0
            then (RCell(intern_var(t,ndx,rtms)), 0)
            else if i = j
            then (RVar(ndx, rtms), free)
            else (RFVar(j, rtms), Int.max(free,j))
          end
        | RCell cell => (RCell cell, 0)
      end

  type term = cell
  datatype term_view = 
    IntLam of term -> term
  | IntConst of cid * term list

  fun view (t as T{term_array, const_to_cid, ...}, cell) = 
      case cell of
        Void => raise Invariant
      | Ndx ndx => 
        let in
          case A.sub term_array ndx of
            ILam(ty,cells) => raise Match
          | IConst(int,cells) => IntConst(cid, 
            
        end

end
