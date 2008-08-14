signature RECON_APPROX = sig

  datatype normal_exp = 
      ExpRule of ExtSyn.rule
    | ExpType of ExtSyn.typ
    | ExpKind of ExtSyn.knd
    | ExpTerm of ExtSyn.trm * Approx.typ

  val parsedsyn_to_extsyn : 
      Pos.pos ParsedSyn.exp -> 
      {exp: normal_exp, freevars: (string * Approx.typ) list}

end

structure ReconApprox :> RECON_APPROX = struct

open Global
structure E = ParsedSyn
structure A = Approx
open ExtSyn

type ctx = (string * A.typ) list

(* PASS 1 (root --> leaves) MARK TERMS WITH CONTEXT/POSITION/TYPE *)

datatype mark = 
    M of {pos: Pos.pos, 
          (* Bound variables w/ inferred approximate types. *)
          ctx: ctx, 
          (* ONLY SIGNIFICANT at binders, this annotation records the inferred 
           * approximate type of the term variable that the binder binds. *)
          arg_ty: A.typ}

type marked_exp = mark E.exp
                
(* val tag_into : ctx -> Pos.pos E.exp -> mark E.exp
 * Mark information from the outside of the term inwards. Instead
 * of just the position, each point in the syntax tree is marked with
 * the context of bound variables and type information. *)
fun tag_into ctx exp : mark E.exp =
    let 
      val (exp,pos) = E.prj exp 
      val arg_ty = A.newTVar()
      val binding =
       fn (E.Var(s,p), e) => (E.Var(s,p), tag_into ((s, arg_ty) :: ctx) e)
        | (E.VarTy((s,p),e'), e) => 
          let val e' = tag_into ctx e'
          in (E.VarTy((s,p),e'), tag_into ((s, arg_ty) :: ctx) e) end
    in 
      E.inj 
          ((case exp of 
              E.Exists b => E.Exists(binding b)
            | E.Pi b     => E.Pi(binding b)
            | E.Lam b    => E.Lam(binding b)
            | exp_view => E.T.map (tag_into ctx) exp_view),
           M{pos = pos, ctx = ctx, arg_ty = arg_ty})
    end 








(* PASS 2 (leaves ---> root) SYNTHESIZE APPROXIMATE TYPES *)

(* PASS 2, PART 1: DATATYPES FOR EXPRESSIONS *)
(* The key to the transformation is that we can work from the leaves outwards,
 * because every syntatic construct can synthesize its unique universe (if any)
 * from the universes of its predececessors. Therefore these datatypes
 * allow us to inject terms that know their universe and (if applicable) their
 * approximate type/kind into a common type. *)

(* datatype exp
 * Special datatypes to collect partially instantiated terms/types
 * 
 * It is important to note that, in this data type, all "spiney" lists of terms
 * are BACKWARDS, that is, c M1 M2 M3 is represented as AtomicTm(c,[M3,M2,M1]),
 * and the get_term and get_type functions must do a list reversal! The 
 * alternative would be to do a lot more list appends. *)
datatype exp = 
    (* Partial instantiated type constants : (c M1 .. Mn : M1 -> ... -> type *)
    AtomicTy of IntSyn.cid * trm list * A.typ list * Global.kind 
  | Rule of rule                                (* R : {eph+, eph-, ...} *)
  | Type of typ list                            (* A : type *)
  | Kind of knd                                 (* K : kind *)
  | AtomicTm of head * trm list * A.typ         (* h M1 ... Mn : A *)
  | RedexTm of (trm * typ) * trm list * A.typ   (* (M: A') M1 ... Mn : A *)
  | NormalTm of (trm * A.typ) list              (* M : A *)

val exp_str =
 fn AtomicTy _ => "partially instantiated atomic type/predicate"
  | Rule _ => "rule"
  | Type [_] => "type"
  | Type _ => "list of types"
  | Kind _ => "type definition"
  | AtomicTm _ => "term"
  | RedexTm _ => "term"
  | NormalTm [_] => "term"
  | NormalTm _ => "list of terms"

(* datatype normal_exps 
 * Normal expressions or conjunctions of expressions. There is 
 * never any need for lists of rules or lists of expressions. *)
datatype normal_exps = 
    ExpsRule of rule
  | ExpsType of typ list
  | ExpsKind of knd
  | ExpsTerm of (trm * A.typ) list

val normal_exps_str : normal_exps -> string =
 fn ExpsRule _ => "rule"
  | ExpsType [_] => "type"
  | ExpsType _ => "list of types"
  | ExpsKind _ => "type definition"
  | ExpsTerm [_] => "term"
  | ExpsTerm _ => "list of terms"

val normal_exps : Pos.pos -> exp -> normal_exps = 
 fn pos =>
 fn AtomicTy(cid, t, [], NONE) => ExpsType [TBase' cid] (* XXX discard args! *)
  | AtomicTy(cid, t, _, NONE) =>
    raise Error("Type not fully instantiated", pos)
  | AtomicTy(cid, t, [], SOME k) => ExpsRule (RAtom'(cid, rev t))
  | AtomicTy(cid, t, _, SOME k) => 
    raise Error("Predicate not fully instantiated",pos)
  | Rule rule => ExpsRule rule
  | Type typs => ExpsType typs
  | Kind knd => ExpsKind knd
  | AtomicTm(h, tms, apx) => ExpsTerm [(MApp'(h, rev tms), apx)]
  | RedexTm(m, tms, apx) => ExpsTerm [(MRedex'(m, rev tms), apx)]
  | NormalTm tms => ExpsTerm tms

(* View normal_exp
 * Single normal expressions *)
datatype normal_exp = 
    ExpRule of rule
  | ExpType of typ
  | ExpKind of knd
  | ExpTerm of trm * A.typ

val normal_exp : Pos.pos -> exp -> normal_exp =
 fn pos =>
 fn exp =>
    case normal_exps pos exp of 
      ExpsRule rule => ExpRule rule
    | ExpsType [typ] => ExpType typ
    | ExpsType _ => raise Error("Expected type, found list of types", pos)
    | ExpsKind knd => ExpKind knd
    | ExpsTerm [trm] => ExpTerm trm
    | ExpsTerm trm => raise Error("Expected term, found list of terms", pos)

val normal_exp_str : normal_exp -> string = 
 fn ExpRule _ => "rule"
  | ExpType _ => "type"
  | ExpKind _ => "type definition"
  | ExpTerm _ => "term"

    

(* PASS 2, PART 2 - PROJECTIONS *)
(* When we require an expression to have a certain form, we can project
 * from the datatype using the get_??? functions *)

val get_rule : Pos.pos -> exp -> rule = 
 fn pos => 
    (fn ExpsRule rule => rule
      | exp => raise Error("Expected rule, found " ^ normal_exps_str exp, pos))
    o normal_exps pos
 
val get_type : Pos.pos -> exp -> typ = 
 fn pos => 
    (fn ExpType typ => typ
      | exp => raise Error("Expected type, found " ^ normal_exp_str exp, pos))
    o normal_exp pos

val get_type_list : Pos.pos -> exp -> typ list =
 fn pos => 
    (fn ExpsType typs => typs
      | exp => raise Error("Expected types, found " ^ normal_exps_str exp, pos))
    o normal_exps pos

val get_kind : Pos.pos -> exp -> knd = 
 fn pos => 
 fn Kind knd => knd
  | exp => raise Error("Expected kind, found " ^ exp_str exp, pos)

val get_term : Pos.pos -> exp -> (trm * A.typ) =
 fn pos =>
    (fn ExpTerm trm => trm
      | exp => raise Error("Expected term, found " ^ normal_exp_str exp, pos))
    o normal_exp pos

val get_term_list : Pos.pos -> exp -> (trm * A.typ) list = 
 fn pos =>
    (fn ExpsTerm trms => trms 
      | exp => raise Error("Expected terms, found " ^ normal_exps_str exp, pos))
    o normal_exps pos


(* PASS 2, PART 3 - VARIABLE/CONSTANT SYNTHESIS *)
(* Bound variables are handled by giving them approximate types.
 * Free variables are handled by generating a map from the free 
 *     variables to their respective types.
 * Signature constants are handled by reading the appropriate classifier
 *     out of the signature. *)

val bound : string -> ctx -> (int * A.typ) option =
 fn var =>
    let fun bound n ctx = 
            case ctx of 
              [] => NONE
            | (x,typ) :: ctx => 
              if var = x then SOME (n, typ) else bound (n+1) ctx
    in bound 0 end

structure MapS = 
SplayMapFn(struct type ord_key = string val compare = String.compare end)

val merge_freevars = 
 fn pos =>
    MapS.unionWithi
        (fn (str, t1, t2) => 
            let in
              A.unify(t1,t2) 
              handle Unify => raise Error("Clashing types for " ^ str, pos);
              t1
            end)

val lookup_const =
 fn pos =>
 fn var =>
    case Sig.string_to_cid var of 
      NONE => raise Error("Undefined variable " ^ var, pos)
    | SOME cid => 
      let val dec = Sig.lookup cid in
        case dec of 
          IntSyn.ConDec{def, ...} => AtomicTm(Const cid, [], A.from_exact def)
        | IntSyn.TypDec{def, ...} => 
          let fun list_knd knd tys = 
                  case IntSyn.K.prj knd of
                    IntSyn.KType k => (rev tys, k)
                  | IntSyn.KArrow(ty,knd) => 
                    list_knd knd (A.from_exact ty :: tys)
            val (tys,knd) = list_knd def []
          in AtomicTy(cid, [], tys, knd) end
        | _ => raise Error("Expected type or kind constant", pos)
      end



(* PASS 2, PART 4 - FOLDING OVER PARSED SYNTAX *)
datatype folder = 
    F of {exp: exp, freevars: A.typ MapS.map}

val fold_decl : A.typ -> folder E.var_decl -> typ =
 fn arg_ty =>
 fn E.Var(s,pos) => TApprox' arg_ty
  | E.VarTy((s, pos), F{exp,freevars}) => 
    let 
      fun unify_typ typ apx = 
          case T.prj typ of 
            TBase cid => A.unify(A.Const' cid, apx)
          | TArrow(typ1,typ2) => 
            let val (apx1, apx2) = A.force_arrow apx 
            in unify_typ typ1 apx1; unify_typ typ2 apx2 end
          | TApprox apx' => A.unify(apx, apx')
      val typ = get_type pos exp
    in 
      if MapS.isEmpty freevars then () 
      else raise Error("Free vars in typ?", pos);
      unify_typ typ arg_ty
      handle Unify =>
           raise Error("Inferred type does not match specification", pos); typ 
    end

val fold : folder E.exp_view * mark -> folder = 
 fn (syn, M{pos,ctx,arg_ty}) =>
    case syn of 
      E.Exists(decl, F{exp,freevars}) => 
      let val var_ty = fold_decl arg_ty decl
      in F{exp = Rule(RExist'(var_ty, get_rule pos exp)),
           freevars = freevars} end

    | E.Pi(decl, F{exp,freevars}) =>
      let val var_ty = fold_decl arg_ty decl in
        F{exp = Rule(RPi'(var_ty, get_rule pos exp)),
           freevars = freevars} 
      end

    | E.Lam(decl, F{exp,freevars}) =>
      let val var_ty = fold_decl arg_ty decl
        val (m,apx) = get_term pos exp
      in F{exp = NormalTm([(MLam'(m), A.Arrow'(arg_ty, apx))]),
           freevars = freevars} end

    | E.App(F{exp = e1,freevars = fv1}, F{exp = e2, freevars = fv2}) =>
      let 
        val freevars =  merge_freevars pos (fv1, fv2)         
        val args : (trm * A.typ) list = get_term_list pos e2
                                        
        (* ty_args: trm list * trm list * A.typ list -> trm list * A.typ list *)
        fun ty_args (current_tms, new_tms, tys) = 
            case (new_tms, tys) of 
              ([], _) => (current_tms, tys)
            | (_, []) => raise Error("Non-function applied to argument", pos)
            | ((tm,apx) :: new_tms, apx' :: tys) =>
              (A.unify(apx, apx') 
               handle Unify => 
                    raise Error("Type of argument did not match", pos); 
               ty_args (tm :: current_tms, new_tms, tys))
              
        (* tm_args: trm list * trm list * A.typ -> trm list * A.typ *)
        fun tm_args (current_tms, new_tms, apx_ty) =
            case new_tms of 
              [] => (current_tms, apx_ty)
            | (tm,apx) :: new_tms =>
              let val (apx1, apx2) = A.force_arrow apx_ty
                   handle A.Unify => 
                          raise Error("Non-function applied to argument", pos)
              in A.unify(apx,apx1)
                 handle Unify => 
                        raise Error("Type of argument did not match", pos);
                 tm_args (tm :: current_tms, new_tms, apx2)
              end

      in (* The cases are pretty simple, for the most part. Either we're 
          * enlarging an atomic term/type, or else we're forming a redex *)
        case e1 of 
          AtomicTy(h, tms, tys, knd) => 
          let val (tms, tys) = ty_args (tms, args, tys) 
          in F{exp = AtomicTy(h, tms, tys, knd), freevars = freevars} end
        | AtomicTm(h, tms, ty) =>
          let val (tms, ty) = tm_args (tms, args, ty)
          in F{exp = AtomicTm(h, tms, ty), freevars = freevars} end
        | RedexTm(m, tms, ty) => 
          let val (tms, ty) = tm_args (tms, args, ty)
          in F{exp = RedexTm(m, tms, ty), freevars = freevars} end
        | NormalTm([(tm, ty)]) =>
          let val (tms, ty) = tm_args ([], args, ty)
          in F{exp = RedexTm((tm, TApprox' ty), tms, ty), 
               freevars = freevars} 
          end
        | _ => raise 
            Error("Expected a term or type, found " ^ exp_str e1, pos)
      end  
        
    | E.Arrow(F{exp = e1, freevars = fv1}, F{exp = e2, freevars = fv2}) =>
      let 
        val freevars = merge_freevars pos (fv1, fv2) in
        case normal_exp pos e2 of 
          ExpRule r2 =>
          F{exp = Rule(RArrow'(get_rule pos e1, r2)),
            freevars = freevars}
        | ExpType t2 =>
          F{exp = Type([foldr TArrow' t2 (get_type_list pos e1)]),
            freevars = freevars}
        | ExpKind k2 =>
          F{exp = Kind(foldr KArrow' k2 (get_type_list pos e1)),
            freevars = freevars}
        | ExpTerm _ => 
          raise Error("Terms not allowed to the right of arrows", pos)
      end

    | E.Pair(F{exp = e1, freevars = fv1}, F{exp = e2, freevars = fv2}) => 
      let val freevars = merge_freevars pos (fv1, fv2) in
        case normal_exps pos e1 of
          ExpsRule r1 =>
          F{exp = Rule(RAnd'(r1, get_rule pos e2)), freevars = freevars}
        | ExpsType tys1 => 
          F{exp = Type(tys1 @ get_type_list pos e2), freevars = freevars}
        | ExpsTerm tms1 =>
          F{exp = NormalTm(tms1 @ get_term_list pos e2), freevars = freevars}
        | ExpsKind _ => 
          raise Error("Type definition not allowd to the left of comma", pos)
      end

    | E.UCid str => 
      let
        val ty = A.newTVar() 
      in
        case bound str ctx of
          NONE => F{exp = AtomicTm(EVar str, [], ty), 
                    freevars = MapS.singleton(str, ty)}
        | SOME(i,apx) => F{exp = AtomicTm(BVar i, [], apx),
                           freevars = MapS.empty}
      end

    | E.LCid str =>
      let in
        F{exp = case bound str ctx of
                  NONE => lookup_const pos str
                | SOME(i,apx) => AtomicTm(BVar i, [], apx),
          freevars = MapS.empty}
      end

    | E.Eq(F{exp = e1, freevars = fv1}, F{exp = e2, freevars = fv2}) => 
      let val freevars = merge_freevars pos (fv1, fv2) 
        val (m1,apx1) = get_term pos e1
        val (m2,apx2) = get_term pos e2
      in
        A.unify(apx1, apx2)
        handle Unify =>
               raise Error("Types of equated terms do not match", pos);
        F{exp = Rule(REq'(m1, m2)),
          freevars = freevars}
      end
      
    | E.Type knd => F{exp = Kind(KType' knd), freevars = MapS.empty} 
                      

val parsedsyn_to_extsyn = 
 fn e =>
    let 
      val (_, pos) = E.prj e
      val F{exp, freevars} = E.fold fold (tag_into [] e)
      val exp = normal_exp pos exp
    in {exp = exp,
        freevars = MapS.listItemsi freevars} end

end
