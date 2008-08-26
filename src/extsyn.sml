structure ExtSyn :> EXT_SYN = struct

datatype head = 
    BVar of int 
  | Const of IntSyn.cid
  | Abbrev of IntSyn.cid
  | FVar of string
  | Omitted of Approx.typ
datatype thead = 
    TConst of IntSyn.cid
  | TAbbrev of IntSyn.cid

datatype ('m,'t) typ_view =
    TBase of thead * 'm list (* No dependent types *)
  | TPi of 't * 't         (* Πx:A.K *)
  | TArrow of 't * 't        (* A -> B *)
  | TApprox of Approx.typ    (* Omitted type, determied by unification *)

datatype ('m,'t) trm_view =
    MLam of 'm              (* Normal: λx.N *)
  | MApp of head * 'm list  (* Atomic: Synthesizing constant and applications *)
  | MRedex of ('m * 't) * 'm list 
                            (* Atomic: Annotated normal term and applications *)
datatype typ = FixTyp of (trm, typ) typ_view
     and trm = FixTrm of (trm, typ) trm_view

datatype 'k knd_view =
    KType of Global.kind    (* type, pers+, pers-, eph+, eph- *)
  | KPi of typ * 'k         (* Πx:A.K *)
  | KArrow of typ * 'k      (* A -> K *)
datatype knd = FixKnd of knd knd_view

datatype 'r rule_view = 
    RAtom of thead * trm list 
  | RExist of typ * 'r
  | RPi of typ * 'r
  | RAnd of 'r list
  | RArrow of 'r * 'r
  | REq of trm * trm
datatype rule = FixRule of rule rule_view


structure MT =
MakeTyp2(struct
         type a = trm
         type b = typ
         type ('m, 't) aview = ('m, 't) trm_view
         type ('m, 't) bview = ('m, 't) typ_view
         val inja = FixTrm
         val injb = FixTyp
         val prja = fn FixTrm t => t
         val prjb = fn FixTyp t => t
         val mapa =
          fn (f,g) =>
          fn MLam t => MLam(f t)
           | MApp(h, ms) => MApp(h, List.map f ms)
           | MRedex((t, typ), ts) => MRedex((f t, g typ), List.map f ts)
         val mapb =
          fn (f,g) =>
          fn TBase(h, ms) => TBase(h, List.map f ms)
           | TArrow(t1, t2) => TArrow(g t1, g t2)
           | TPi(t1, t2) => TPi(g t1, g t2)
           | TApprox approx => TApprox approx
         end)

structure M = MT.FullA
structure T = MT.FullB 

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
         fn RAtom(h, ts) => RAtom(h, ts)
          | RExist(typ, t) => RExist(typ, f t)
          | RPi(typ, t) => RExist(typ, f t)
          | RAnd(ts) => RAnd(List.map f ts)
          | RArrow(t1,t2) => RArrow(f t1, f t2)
          | REq tms => REq tms
        end)

val TBase' = T.inj o TBase
val TArrow' = T.inj o TArrow
val TPi' = T.inj o TPi
val TApprox' = T.inj o TApprox
val MLam' = M.inj o MLam
val MApp' = M.inj o MApp
val MRedex' = M.inj o MRedex
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
      TBase(TConst cid, _) => Approx.Const' cid
    | TBase(TAbbrev cid, _) => Approx.Const' cid
    | TArrow(t1,t2) => Approx.Arrow'(typ_to_apx t1, typ_to_apx t2)
    | TPi(t1,t2) => Approx.Arrow'(typ_to_apx t1, typ_to_apx t2)
    | TApprox(apx) => apx


end
