signature APPROX = sig
    
  type tvar 
  type typ
  datatype 't typ_view =
      Unknown of tvar
    | Arrow of 't * 't 
    | Const of IntSyn.cid

  include TYP_FULL
   where type T.t = typ
     and type 't T.view = 't typ_view
 
  exception NotAType of string
  exception Unify

  val unify : typ * typ -> unit
  val from_exact : IntSyn.typ -> typ
  val to_string : typ -> string

  (* force_arrow asserts that the argument type is an arrow, and returns 
   * the subterms. *)
  val force_arrow : typ -> typ * typ  
  val newTVar : unit -> typ

  val Unknown' : tvar -> typ
  val Arrow' : (typ * typ) -> typ
  val Const' : IntSyn.cid -> typ

end

structure Approx :> APPROX = struct

open IntSyn

datatype 't typ_view =
    Unknown of tvar 
  | Arrow of 't * 't 
  | Const of cid

and tvar = TV of typ option ref

and typ = FixTyp of typ typ_view


exception NotAType of string
fun inj (Const cid) = 
    let in
      case Sig.lookup cid of
        IntSyn.TypDec _ => FixTyp(Const cid)
      | IntSyn.TypAbbrev {typ, ...} => from_exact typ
      | _ => raise NotAType (Sig.cid_to_string cid)
    end
  | inj obj = FixTyp obj
              
and from_exact obj = 
    IntSyn.T.fold 
        (fn TBase(cid,_) => inj(Const cid)
          | TPi(_,t1,t2) => inj(Arrow(t1,t2))) obj
 

(* Unification is dumb - make better union-find! *)
structure A = 
MakeTyp(struct
        type t = typ
        type 't view = 't typ_view
        val inj = inj
        fun prj (FixTyp obj) = 
            case obj of 
              Unknown(TV(r as ref(SOME typ))) => 
              let val ans = prj typ 
              in r := SOME(FixTyp ans); ans end (* path compression! *)
            | obj => obj
        fun map f obj =
            case obj of
              Unknown tv => Unknown tv
            | Arrow(t1,t2) => Arrow(f t1, f t2)
            | Const(cid) => Const(cid)
        end)
open A

exception Unify
val newTVar = fn () => inj(Unknown(TV(ref NONE)))
fun unify (a,b) = 
    case (A.prj a, A.prj b) of
      (Unknown(TV(a)), _) => a := SOME b
    | (_, Unknown(TV(b))) => b := SOME a
    | (Arrow(a1,a2), Arrow(b1,b2)) => (unify(a1,b1); unify(a2,b2))
    | (Const c1, Const c2) => if Cid.equal(c1,c2) then () else raise Unify
    | _ => raise Unify
fun force_arrow a = 
    case A.prj a of
      Arrow(a,b) => (a,b)
    | Unknown(TV(r)) => 
      let val (a,b) = (newTVar(), newTVar())
      in r := SOME(inj(Arrow(a,b))); (a,b) end
    | _ => raise Unify

val Unknown' = inj o Unknown
val Const' = inj o Const
val Arrow' = inj o Arrow
val to_string = 
    fold (fn Unknown _ => "?" 
           | Const cid => Sig.cid_to_string cid
           | Arrow(s1,s2) => s1 ^ " -> " ^ s2)

end
