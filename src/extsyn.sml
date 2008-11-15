structure ExtSyn :> EXT_SYN = struct

datatype head = BVar of int | Const of IntSyn.cid | EVar of string

datatype 't typ_view =
    TBase of IntSyn.cid     (* No dependent types *)
  | TArrow of 't * 't       (* A -> B *)
  | TApprox of Approx.typ   (* Omitted type, determied by unification *)
datatype typ = FixTyp of typ typ_view

datatype 'm trm_view =
    MLam of 'm              (* Normal: λx.N *)
  | MApp of head * 'm list  (* Atomic: Synthesizing constant and applications *)
  | MRedex of ('m * typ) * 'm list 
                            (* Atomic: Annotated normal term and applications *)
  | MUnknown of typ * 'm list
                            (* Term not mentioned (underscore) *)
datatype trm = FixTrm of trm trm_view

datatype 'k knd_view =
    KType of Global.kind    (* type, pers+, pers-, eph+, eph- *)
  | KPi of typ * 'k         (* Πx:A.K *)
  | KArrow of typ * 'k      (* A -> K *)
datatype knd = FixKnd of knd knd_view

datatype 'r rule_view = 
    RAtom of IntSyn.cid * trm list 
  | RExist of typ * 'r
  | RPi of typ * 'r
  | RAnd of 'r list
  | RArrow of 'r * 'r
  | REq of trm * trm
datatype rule = FixRule of rule rule_view


structure M = 
MakeTyp(struct
        type t = trm
        type 't view = 't trm_view
        val inj = FixTrm
        val prj = fn FixTrm t => t
        val map =
         fn f =>
         fn MLam t => MLam(f t)
          | MApp(h, ts) => MApp(h, List.map f ts)
          | MRedex((t, typ), ts) => MRedex((f t, typ), List.map f ts)
          | MUnknown(typ, ts) => MUnknown(typ, List.map f ts)
        end)

structure T = 
MakeTyp(struct
        type t = typ
        type 't view = 't typ_view
        val inj = FixTyp
        val prj = fn FixTyp t => t
        val map =
         fn f =>
         fn TBase cid => TBase cid
          | TArrow(t1, t2) => TArrow(f t1, f t2)
          | TApprox approx => TApprox approx
        end)

structure K = 
MakeTyp(struct
        type t = knd
        type 't view = 't knd_view
        val inj = FixKnd
        val prj = fn FixKnd t => t
        val map = 
         fn f =>
         fn KType kind => KType kind
          | KPi(typ, t) => KPi(typ, f t)
          | KArrow(typ, t) => KArrow(typ, f t)
        end)

structure R =
MakeTyp(struct
        type t = rule
        type 't view = 't rule_view
        val inj = FixRule
        val prj = fn FixRule t => t
        val map = 
         fn f =>
         fn RAtom(cid, ts) => RAtom(cid, ts)
          | RExist(typ, t) => RExist(typ, f t)
          | RPi(typ, t) => RExist(typ, f t)
          | RAnd(ts) => RAnd(List.map f ts)
          | RArrow(t1,t2) => RArrow(f t1, f t2)
          | REq tms => REq tms
        end)

val TBase' = T.inj o TBase
val TArrow' = T.inj o TArrow
val TApprox' = T.inj o TApprox
val MLam' = M.inj o MLam
val MApp' = M.inj o MApp
val MRedex' = M.inj o MRedex
val MUnknown' = M.inj o MUnknown
val KType' = K.inj o KType
val KPi' = K.inj o KPi
val KArrow' = K.inj o KArrow
val RAtom' = R.inj o RAtom
val RExist' = R.inj o RExist
val RPi' = R.inj o RPi
val RAnd' = 
 fn (FixRule(RAnd e1), FixRule(RAnd e2)) => FixRule(RAnd(e1 @ e2))
  | (FixRule(RAnd e1), e2) => FixRule(RAnd(e1 @ [e2]))
  | (e1, FixRule(RAnd e2)) => FixRule(RAnd(e1 :: e2))
  | (e1, e2) => FixRule(RAnd [e1,e2])
val RArrow' = R.inj o RArrow
val REq' = R.inj o REq

fun typ_to_apx typ = 
    case T.prj typ of
      TBase cid => Approx.Const' cid
    | TArrow(t1,t2) => Approx.Arrow'(typ_to_apx t1, typ_to_apx t2)
    | TApprox(apx) => apx


end
