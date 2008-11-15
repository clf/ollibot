signature EXT_SYN = sig

(* EVars aren't lowered until they are transformed into the internal syntax *)
datatype head = BVar of int | Const of IntSyn.cid | EVar of string

type typ 
datatype 't typ_view =
    TBase of IntSyn.cid     (* No dependent types *)
  | TArrow of 't * 't       (* A -> B *)
  | TApprox of Approx.typ   (* Omitted type, determied by unification *)

type trm
datatype 'm trm_view =
    MLam of 'm              (* Normal: λx.N *)
  | MApp of head * 'm list  (* Atomic: Synthesizing constant and applications *)
  | MRedex of ('m * typ) * 'm list 
                            (* Atomic: Annotated normal term and applications *)
  | MUnknown of typ * 'm list
                            (* Term not mentioned (underscore) *)

type knd
datatype 'k knd_view =
    KType of Global.kind    (* type, pers+, pers-, eph+, eph- *)
  | KPi of typ * 'k         (* Πx:A.K *)
  | KArrow of typ * 'k      (* A -> K *)

type rule 
datatype 'r rule_view = 
    RAtom of IntSyn.cid * trm list 
  | RExist of typ * 'r
  | RPi of typ * 'r
  | RAnd of 'r list
  | RArrow of 'r * 'r
  | REq of trm * trm

structure M : TYP_FULL
 where type T.t = trm
   and type 'm T.view = 'm trm_view

structure T : TYP_FULL
 where type T.t = typ
   and type 't T.view = 't typ_view

structure K : TYP_FULL
 where type T.t = knd
   and type 'k T.view = 'k knd_view

structure R : TYP_FULL
 where type T.t = rule
   and type 'r T.view = 'r rule_view

val TBase' : IntSyn.cid -> typ        
val TArrow' : typ * typ -> typ      
val TApprox' : Approx.typ -> typ
val MLam' : trm -> trm               
val MApp' : head * trm list -> trm  
val MRedex' : (trm * typ) * trm list -> trm
val MUnknown' : typ * trm list -> trm
val KType' : Global.kind -> knd
val KPi' : typ * knd -> knd
val KArrow' : typ * knd -> knd
val RAtom' : IntSyn.cid * trm list -> rule
val RExist' : typ * rule -> rule 
val RPi' : typ * rule -> rule
val RAnd' : rule * rule -> rule
val RArrow' : rule * rule -> rule
val REq' : trm * trm -> rule

val typ_to_apx : typ -> Approx.typ

end
