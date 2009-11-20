(* Ollibot — Robert J. Simmons and Frank Pfenning
 * Simple type inference and *)


structure SimpleType :> 
          sig
            type evar
            type styp
            datatype styp_view = 
                     Var of evar | Item | Prop | Arrow of styp * styp
            exception Unify
            val unify : styp -> styp -> unit
            val bind : evar -> styp -> unit
            val prj : styp -> styp_view
            val Var' : unit -> styp
            val Item' : styp
            val Prop' : styp
            val Arrow' : styp * styp -> styp

            (* Debugging only! *)
            val isItem : styp -> bool
            val isProp : styp -> bool
          end =
struct

  open Global

  datatype evar = E of styp_view option ref
  and styp_view = Var of evar | Item | Prop | Arrow of styp_view * styp_view

  type styp = styp_view
              
  fun prj typ = 
      let
        fun lookup (v as ref NONE) = Var(E v)
          | lookup (v as ref (SOME(Var(E v')))) = 
            let val typ = lookup v' 
            in v := SOME typ; typ end
          | lookup (v as ref (SOME typ)) = typ
      in
        case typ of
          Item => Item
        | Prop => Prop
        | Arrow(t1,t2) => Arrow(t1,t2)
        | Var(E v) => lookup v
      end

  fun bind (E(v as ref NONE)) trm =
      (case prj trm of
         Var(E v') => if v = v' then () else v := SOME trm
       | _ => v := SOME trm)
    | bind _ _ = raise Match

  val Var' = fn () => Var(E(ref NONE))
  val Item' = Item
  val Prop' = Prop
  val Arrow' = Arrow

  fun occurs_check (e1 : evar) t2 = 
      case t2 of
        Var e2 => if e1 = e2 then raise Err("Cannon assign types") else ()
      | Item => ()
      | Prop => ()
      | Arrow(t1,t2) => (occurs_check e1 t1; occurs_check e1 t2)

  exception Unify
  fun unify t1 t2 = 
      case (prj t1, prj t2) of
        (Var e1, t2) => bind e1 t2
      | (t1, Var e2) => bind e2 t1
      | (Item, Item) => ()
      | (Prop, Prop) => ()
      | (Arrow(t1,s1),Arrow(t2,s2)) => (unify t1 t2; unify s1 s2)
      | _ => raise Unify

  val isItem = fn Item => true | _ => false
  val isProp = fn Prop => true | _ => false
      
end

structure ExtSyn = struct

  datatype term = 
      App of Pos.pos * term * term  
    | Forall of Pos.pos * SimpleType.styp * string * term
    | Exists of Pos.pos * SimpleType.styp * string * term
    | Unit of Pos.pos
    | Conj of Pos.pos * term * term
    | Lolli of Pos.pos * term * term
    | Lambda of Pos.pos * SimpleType.styp * string * term
    | Id of Pos.pos * string list * string
    | Bang of Pos.pos * term
    | Eq of Pos.pos * term * term
    | Neq of Pos.pos * term * term

  fun getpos term = 
      case term of
        App(p,_,_) => p
      | Forall(p,_,_,_) => p    
      | Exists(p,_,_,_) => p
      | Unit(p) => p
      | Conj(p,_,_) => p
      | Lolli(p,_,_) => p
      | Lambda(p,_,_,_) => p
      | Id(p,_,_) => p
      | Bang(p,_) => p
      | Eq(p,_,_) => p
      | Neq(p,_,_) => p


  datatype decl = 
      RULE of Pos.pos * string * term
    | EXEC of Pos.pos * int option * term
    | TRACE of Pos.pos * int option * term
              
end
