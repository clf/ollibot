signature RECON_EXACT = sig

  datatype normal_exp = 
      ExpKind of IntSyn.knd
    | ExpRule of IntSyn.neg
    | ExpType of IntSyn.typ 
    | ExpTerm of IntSyn.trm * IntSyn.typ

  val extsyn_to_intsyn : 
      Pos.pos ->
      {exp: ReconApprox.normal_exp, freevars: (string * Approx.typ) list} ->
      {exp: normal_exp, 
       freevars: (string * IntSyn.typ) list,
       evars: (IntSyn.evar * IntSyn.ctx * IntSyn.typ) list}

end

structure ReconExact :> RECON_EXACT= struct

open Global
structure E = ExtSyn
structure A = Approx
open IntSyn

datatype normal_exp = 
    ExpKind of IntSyn.knd
  | ExpRule of IntSyn.neg
  | ExpType of IntSyn.typ 
  | ExpTerm of IntSyn.trm * IntSyn.typ

structure MapS = 
SplayMapFn(struct type ord_key = string val compare = String.compare end)
structure MapE = SplayMapFn(EVar)


val MLam' = M.inj o MLam
val MBase' : (head * trm list) -> typ -> trm = fn _ => raise Match
val MRedex' : (trm * trm list) -> trm = fn _ => raise Match
val TPi' = fn(t1, t2) => T.inj(TPi(Pi, t1, t2))
val TArrow' = fn (t1, t2) => T.inj(TPi(Arrow, t1, t2))
val KPi' = fn(t1, k2) => K.inj(KPi(Pi, t1, k2))
val KArrow' = fn (t1, k2) => T.inj(KPi(Arrow, t1, k2))

exception Internal of string

fun get_TPi t = case T.prj t of TPi x => x | _ => raise Internal "TPi"
fun get_KPi k = case K.prj k of KPi x => x | _ => raise Internal "KPi"

fun extsyn_to_intsyn pos {exp = exp, freevars = freevars} =
    let

      val evars : (IntSyn.ctx * IntSyn.typ) MapE.map ref = ref MapE.empty

      (* creatEVar : {G: ctx} * typ G -> trm G *)
      fun create_EVar(G, t) = 
          case T.prj t of
            TBase(cid, trm) => 
            let
              val evar = newEVar()
            in
              evars := MapE.insert(!evars,evar,(G,t));
              evar
            end
          | TPi(dep, t1, t2) => MLam'(create_EVar(t1 :: G, t2))

      (* create_EVar_spine : {G: ctx} * knd G -> (trm G) list *)
      (* Given a context and a spine defined in that context, return a *)
      (* spine of evars *)
      fun create_EVar_spine (G, k) ms = 
          case K.prj k of 
            KType _ => rev ms
          | KPi(_, t1, k2) => 
            let 
              val m = create_EVar(G, t1) 
            in 
              raise Match
              (* create_EVar_spine (G, k2) (create_EVar(G, t1) :: ms *)
            end

      fun approx_typ (G, apx_t) = 
          case apx_t of
            A.Unknown _ => raise Global.Error("Unresolved type", pos)
          | A.Arrow(apx_t1, apx_t2) =>
            let val t1 = approx_typ (G, apx_t1) 
            in TPi'(t1, approx_typ (t1 :: G, apx_t2)) end
          | A.Const cid =>
            let val {id, knd} = Sig.lookup_TypDec cid 
            in raise Match end

      fun unify_typ (t1, t2) = ()
      fun unify_trm (m1, m2) = ()
      fun check_typ _ = ()
                        
      (* fun check_spine : 
             (∀G. class G -> typ G * class (G, x)) *
             (∀G. class (G, x) * trm G -> class G) *
             (∀G. ) ->
             ({G: ctx} * E.spine G * class G) -> (spine G * class G) *)
      fun check_spine (get_pi, subst, check_trm) (G, ms, class)  = 
          let 
            fun check (G, [], class) ms' = (rev ms', class)
              | check (G, m :: ms, class) ms' =
                let 
                  val (dep, ty, class) = get_pi class
                  val m' = check_trm (G, m, ty) 
                  val class' = subst (class, m')
                in check (G, ms, class') (m' :: ms') end
          in check (G, ms, class) [] end   

      fun check_spine_typ (G, ms, ty) = 
          check_spine (get_TPi, (fn _ => raise Match), check_trm) (G, ms, ty)
      and check_spine_knd (G, ms, ty) = 
          check_spine (get_KPi, (fn _ => raise Match), check_trm) (G, ms, ty)
          
      (* check_trm : {G: ctx} * E.trm G * typ G -> trm G *) 
      and check_trm (G : ctx, m : E.trm, t : typ) : trm =
          case (E.M.prj m, T.prj t) of 
            (E.MLam(NONE, m), TPi(dep, t1, t2)) =>
            let val m' = check_trm (t1 :: G, m, t2) 
            in MLam' m' end
          | (E.MLam(SOME type_ascription, m), TPi(dep, t1, t2)) =>
            let 
              val type_ascription' = check_typ (G, type_ascription)
              val m' = check_trm (t1 :: G, m, t2)
            in
              unify_typ(type_ascription', t1);
              MLam' m' 
            end
          | (E.MApp(E.BVar k, ms), _) => 
            let
              val typ = List.nth(G, k) 
              val (ms', typ') = check_spine_typ (G, ms, typ)
            in
              unify_typ(typ', t);
              MBase' (BVar k, ms') typ'
            end
          | (E.MApp(E.Const cid, ms), _) =>
            let
              val {id, typ} = Sig.lookup_ConDec cid
              val (ms', typ') = check_spine_typ (G, ms, typ)
            in
              unify_typ(typ', t);
              MBase' (Const cid, ms') typ' 
            end
          | (E.MApp(E.Abbrev cid, ms), _) =>
            let 
              val {id, trm, typ} = Sig.lookup_ConAbbrev cid
              val (ms', typ') = check_spine_typ (G, ms, typ)
            in
              unify_typ (typ', t);
              MRedex' (trm, ms') 
            end
    in raise Match end

end
