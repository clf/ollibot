signature INT_SYN = sig

type cid

(* Classifiers for terms trm are types typ 
 * Classifiers for types typ are just the single kind "type" *)
datatype head = BVar of int | Const of cid
type trm
datatype 'm subst = 
    SubIdx of int * 'm subst 
  | SubTrm of 'm * 'm subst 
  | SubShift of int
datatype 'm trm_view = 
    MBase of head * 'm list
  | MLam of 'm
  | MVar of string * 'm subst

type typ 
datatype 't typ_view =
    TBase of cid
  | TArrow of 't * 't

(* Classifiers for proof objects (no datatype) are rules pos and neg
 * Classifiers for rules rul are rule predicates pred. 
 * in
 * If we had a true logical framework, then predicates pred would be kinds
 * knd, rules rul would be types typ, and terms trm would be proof objects. *)

type knd
datatype 'p knd_view =
    KType of Global.kind
  | KArrow of typ * 'p

type pos
type neg
datatype ('p, 'n) pos_view =
    PAtom of cid * trm list 
  | PExist of typ * 'p
  | PAnd of 'p * 'p
  | PNeg of 'n

datatype ('p, 'n) neg_view =
    NAtom of cid * trm list
  | NForall of typ * 'n
  | NArrow of 'p * 'n
  | NPos of 'p

datatype dec = 
    ConDec    of {id: string, typ: typ}            (* c : At : type     *)
  | ConAbbrev of {id: string, trm: trm, typ: typ}  (* d : At = M : kind *)
  | TypDec    of {id: string, knd: knd}            (* a : Kr : kind     *)
  | TypAbbrev of {id: string, typ: typ, knd: knd}  (* a : Kr = A : kind *)
  | PosDec    of {id: string, pos: pos}            (* _ : A+ : p/e+     *)
  | NegDec    of {id: string, neg: neg}            (* _ : A- : p/e-     *)

val dec_id : dec -> string

(* Signature *)
structure Cid : sig
  type ord_key = cid
  val compare : (cid * cid) -> order
  val equal : (cid * cid) -> bool
end

type signat

val sgnEmpty : signat
val sgnLookup : signat * cid -> dec
val sgnAdd : signat * dec -> signat * cid

(* Implementation of recursion schemes *)

structure M : TYP_FULL
 where type T.t = trm
   and type 'm T.view = 'm trm_view

structure T : TYP_FULL
 where type T.t = typ
   and type 't T.view = 't typ_view

structure K : TYP_FULL
 where type T.t = knd
   and type 'k T.view = 'k knd_view

structure P : TYP2_FULLA
 where type a = pos
   and type b = neg
   and type ('p, 'n) aview = ('p, 'n) pos_view
   and type ('p, 'n) bview = ('p, 'n) neg_view

structure N : TYP2_FULLB
 where type a = pos
   and type b = neg
   and type ('p, 'n) aview = ('p, 'n) pos_view
   and type ('p, 'n) bview = ('p, 'n) neg_view

end
