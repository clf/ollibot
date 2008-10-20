structure IntSyn :> INT_SYN = struct

open Global
type cid = int

datatype head = BVar of int | Const of cid | FVar of string
datatype 'm subst = 
    SubIdx of int * 'm subst 
  | SubTrm of 'm * 'm subst 
  | SubShift of int
datatype 'm trm_view = 
    MBase of head * 'm list
  | MLam of 'm
  | MVar of evar * 'm subst
and trm = FixTrm of trm trm_view
and evar = EV of int * trm option ref 

val newEVar = 
    let val i = ref 0 
    in fn () => (i := !i + 1; EV(!i, ref NONE)) end
structure EVar = 
struct type ord_key = evar
val compare = fn (EV(i1,_), EV(i2,_)) => Int.compare(i1,i2) 
end

datatype depend = Arrow | Pi
datatype 't typ_view =
    TBase of cid * trm list
  | TPi of depend * 't * 't
datatype typ = FixTyp of typ typ_view
type spine = trm list
type ctx = typ list

datatype 'p knd_view =
    KType of kind
  | KPi of depend * typ * 'p
datatype knd = FixKnd of knd knd_view

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
datatype pos = FixPos of (pos, neg) pos_view
     and neg = FixNeg of (pos, neg) neg_view

datatype dec = 
    ConDec    of {id: string, typ: typ}            (* c : At : type     *)
  | ConAbbrev of {id: string, typ: typ, trm: trm}  (* d : At = M : type *)
  | TypDec    of {id: string, knd: knd}            (* a : Kr : kind     *)
  | TypAbbrev of {id: string, knd: knd, typ: typ}  (* a : Kr = A : kind *)
  | PosDec    of {id: string, pos: pos}            (* _ : A+ : p/e+     *)
  | NegDec    of {id: string, neg: neg}            (* _ : A- : p/e-     *)
val dec_id = 
 fn ConDec {id,...} => id
  | ConAbbrev {id,...} => id
  | TypDec {id,...} => id
  | TypAbbrev {id,...} => id
  | PosDec {id,...} => id
  | NegDec {id,...} => id


structure Cid = struct type ord_key = int val compare = Int.compare 
                val equal = fn (e1: int,e2) => e1 = e2 end
structure MapI = SplayMapFn(Cid)

type signat = dec MapI.map

val sgnEmpty = MapI.empty
val sgnLookup = MapI.lookup
val sgnAdd = 
    let val max = ref 0 
    in fn (signat, dec : dec) => 
          (max := !max + 1; (MapI.insert(signat,!max,dec), !max))
    end

structure M = 
MakeTyp (struct
         type t = trm
         type 'm view = 'm trm_view
         fun inj obj = FixTrm obj
         fun prj (FixTrm obj) = obj
         fun submap f sub = 
             case sub of 
               SubIdx(i,sub) => SubIdx(i,submap f sub)
             | SubTrm(m,sub) => SubTrm(f m, submap f sub)
             | SubShift i => SubShift i
         fun map f obj = 
             case obj of 
               MBase(h,s) => MBase(h,List.map f s)
             | MLam(m) => MLam(f m)
             | MVar(s,sub) => MVar(s, submap f sub)
         end)

structure T =
MakeTyp (struct
         type t = typ
         type 't view = 't typ_view
         fun inj obj = FixTyp obj
         fun prj (FixTyp obj) = obj
         fun map f obj = 
             case obj of 
               TBase cid => TBase cid
             | TPi(d,t1,t2) => TPi(d, f t1, f t2)
         end)

structure K = 
MakeTyp (struct
         type t = knd
         type 'k view = 'k knd_view
         val inj = FixKnd
         val prj = fn FixKnd knd => knd
         val map = 
          fn f =>
          fn KType k => KType k
           | KPi(d,t,k) => KPi(d,t, f k)
         end)

structure PN = 
MakeTyp2(struct
         type a = pos
         type b = neg
         type ('p, 'n) aview = ('p, 'n) pos_view
         type ('p, 'n) bview = ('p, 'n) neg_view
         fun inja pos = FixPos pos
         fun prja (FixPos pos) = pos             
         fun injb neg = FixNeg neg
         fun prjb (FixNeg neg) = neg
         fun mapa (fpos, fneg) pos = 
             case pos of 
               PAtom(cid, ms) => PAtom(cid, ms)
             | PExist(a, x) => PExist(a, fpos x)
             | PAnd(x1, x2) => PAnd(fpos x1, fpos x2)
             | PNeg(y1) => PNeg(fneg y1)
         fun mapb (fpos, fneg) neg = 
             case neg of
               NAtom(cid, ms) => NAtom(cid, ms)
             | NForall(a, y) => NForall(a, fneg y)
             | NArrow(x, y) => NArrow(fpos x, fneg y)
             | NPos(x) => NPos(fpos x)
         end)

structure P = PN.FullA
structure N = PN.FullB

end
