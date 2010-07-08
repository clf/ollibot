structure TypeRecon = struct
  
  open Global

  structure ST = SimpleType
  structure E = ExtSyn
  structure I = IntSyn

  (* Type approximation and abstracting over free variables 
   * 
   * In this stage, we determine all types in the file, and discover any
   * type errors. 
   * 
   *** Types are determined by unification using the SimpleType structure
   *** Free variables are determined by 
   *)
  fun infertypes_decl (signat,decl) = 
      let 

        (* This map stores the inferred global signature for constants *)
        val constmap : ST.styp MapS.map ref = ref MapS.empty
        fun ItoST typ = 
            case typ of
              I.Prop => ST.Prop'
            | I.Item => ST.Item'
            | I.Arrow(typ1,typ2) => ST.Arrow'(ItoST typ1, ItoST typ2)
        fun lookup s = 
            case MapS.find(signat,s) of
              SOME t => ItoST t
            | NONE => 
              (case MapS.find(!constmap,s) of
                 SOME t => t
               | NONE => 
                 let val t = ST.Var'()
                 in constmap := MapS.insert(!constmap,s,t); t end)
        fun unify (x,y) = ST.unify x y
        fun unifypos (p,x,y) = 
            ST.unify x y 
            handle ST.Unify => 
                   raise ErrPos(p,"Incompatible types for subterms") 

        val union = 
            fn (p,fvar1,fvar2) =>
               MapS.unionWith (fn (t1,t2) => (unify(t1,t2); t1)) (fvar1,fvar2)
               handle ST.Unify => 
                      raise ErrPos(p,"Incompatible types for free variables") 

        (* vars_and_types(trm, bvar) = (pos, fvar, tp) 
         ** trm - a term in the external syntax
         ** bvar - a map from bound variables to their inferred types 
         ** fvar - a set of free variables within the term trm
         ** tp - an inferred type for the term trm *)
        fun vars_and_types (trm, bvar) = 
            case trm of 
              E.App(p,trm1,trm2) =>
              let (* val _ = print "App\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
                val (p1,fvar2,tp2) = vars_and_types (trm2, bvar)
                val tp = ST.Var'()
                val fvar = union (p,fvar1,fvar2)
              in 
                unifypos(p,tp1,ST.Arrow'(tp2,tp)); (p, fvar, tp)
              end
            | E.Forall(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (p0,fvar0,tp0) 
                  = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in unifypos(p,tp0,ST.Prop'); (p, fvar0, ST.Prop') end
            | E.Exists(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (p0,fvar0,tp0)
                  = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in unifypos(p,tp0,ST.Prop'); (p, fvar0, ST.Prop') end
            | E.Fuse(p,trm1,trm2) =>
              let (* val _ = print "Fuse\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
                val (p1,fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = union (p,fvar1,fvar2)
              in
                unifypos(p,tp1,ST.Prop');
                unifypos(p,tp2,ST.Prop'); 
                (p, fvar, ST.Prop')
              end
            | E.Esuf(p,trm1,trm2) =>
              let (* val _ = print "Fuse\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
                val (p2,fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = union (p,fvar1,fvar2)
              in
                unifypos(p,tp1,ST.Prop'); 
                unifypos(p,tp2, ST.Prop'); 
                (p, fvar, ST.Prop')
              end
            | E.Righti(p,trm1,trm2) =>
              let (* val _ = print "Righti\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
                val (p2,fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = union (p,fvar1,fvar2)
              in
                unifypos(p,tp1,ST.Prop'); 
                unifypos(p,tp2,ST.Prop');
                (p, fvar, ST.Prop')
              end
            | E.Lefti(p,trm1,trm2) =>
              let (* val _ = print "Lefti\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
                val (p2,fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = union (p,fvar1,fvar2)
              in
                unifypos(p,tp1,ST.Prop');
                unifypos(p,tp2,ST.Prop'); 
                (p, fvar, ST.Prop')
              end
            | E.Lambda(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (p0,fvar0,tp0)
                  = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in (p, fvar0, ST.Arrow'(tp,tp0)) end
            | E.Id(p,[],x) =>
              let (* val _ = print ("Id " ^ x ^ "\n") *) in
                case MapS.find(bvar, x) of
                  SOME tp => (* Bound variable, obtain from bvars *)
                  (p, MapS.empty, tp)
                | NONE =>
                  if Char.isUpper(String.sub(x,0)) 
                  then (* Free variable, bind locally *)
                    let val tp = ST.Var'() 
                    in (p, MapS.singleton(x,tp), tp) end
                  else (* Constant, get type globally *)
                    let val tp = lookup x
                    in (p, MapS.empty, tp) end
              end
            | E.Id(p,_,x) => raise ErrPos(p,"Paths not yet supported!")
            | E.Bang(p,trm1) =>
              let (* val _ = print "Bang\n" *)
                val (p1, fvar1,tp1) = vars_and_types (trm1, bvar)
              in
                unifypos(p,tp1,ST.Prop'); 
                (p, fvar1, ST.Prop')
              end
            | E.Gnab(p,trm1) =>
              let (* val _ = print "Gnab\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
              in
                unifypos(p,tp1,ST.Prop');
                (p, fvar1, ST.Prop')
              end
            | E.Not(p,trm1) =>
              let (* val _ = print "Bang\n" *)
                val (p1,fvar1,tp1) = vars_and_types (trm1, bvar)
              in
                unifypos(p,tp1,ST.Prop'); 
                (p,fvar1, ST.Prop')
              end
            | E.One p => (p, MapS.empty, ST.Prop')
            | _ => (print "Error - what's missing??!?!?!"; raise Match)

        (* Print a partially inferred type *)
        fun strST tp needs_parens = 
            case ST.prj tp of
              ST.Var ev => "#"
            | ST.Item => "i"
            | ST.Prop _ => "o"
            | ST.Arrow(t1,t2) =>
              let val s = strST t1 true ^ " → " ^ strST t2 false
              in if needs_parens then "(" ^ s ^ ")" else s end 
                                                         
        fun learntypes (E.RULE(p,s,trm)) = 
            let val (p,fv,tp) = vars_and_types(trm,MapS.empty)
              fun wrap (fv, tp, trm) = E.Forall(p,tp,fv,trm)
            in
              ST.unify tp ST.Prop'; 
              E.RULE(p,s,MapS.foldri wrap trm fv)
            end
          | learntypes (E.EXEC(p,n,trm)) = 
            let val (p,fv,tp) = vars_and_types(trm,MapS.empty)
            in 
              ST.unify tp ST.Prop'; 
              if MapS.isEmpty fv then E.EXEC(p,n,trm)
              else raise Match (* XXX needs error message *)
            end
          | learntypes (E.TRACE(p,n,trm)) = 
            let val (p,fv,tp) = vars_and_types(trm,MapS.empty)
            in 
              ST.unify tp ST.Prop'; 
              if MapS.isEmpty fv then E.TRACE(p,n,trm)
              else raise Match (* XXX needs error message *)
            end

        val closed_decl = learntypes decl

        fun groundST tp = 
            case ST.prj tp of
              ST.Var ev => (ST.bind ev ST.Item'; I.Item)
            | ST.Arrow(t1,t2) => I.Arrow(groundST t1, groundST t2)
            | ST.Item => I.Item
            | ST.Prop perm => I.Prop 

        val new_signat =
            MapS.unionWith (fn _ => raise Err("Cannot merge signatures"))
                           (signat, MapS.map groundST (!constmap))

      in (new_signat, closed_decl) end
 
  fun transform_decl (signat, decl) = 
      let 
        datatype var = V_MVar | V_Var
        fun lookup_var (x : string) vars (u,i) =
            case vars of 
              [] => NONE
            | (V_MVar, y, tp) :: vars => 
              if x = y then SOME (V_MVar, u, tp) else lookup_var x vars (u+1,i)
            | (V_Var, y, tp) :: vars =>
              if x = y then SOME (V_Var, i, tp) else lookup_var x vars (u,i+1)

        (* We need to be able to turn a partial spine into a total spine *)
        fun make_spine tp =
            case tp of
              I.Prop => (0,[])
            | I.Item => (0,[])
            | I.Arrow(tp1,tp2) => 
              let val (i,spine) = make_spine tp2
              in (i+1, I.eta_expand_head tp1 (I.Var i) :: spine) end

        (* Check that a type is ground, return the internal type *)
        exception CouldNotInferType
        fun is_groundST tp = 
            case ST.prj tp of
              ST.Var ev => raise CouldNotInferType
            | ST.Arrow(tp1,tp2) => I.Arrow(is_groundST tp1, is_groundST tp2)
            | ST.Item => I.Item
            | ST.Prop perm => I.Prop

        datatype partial_term_root =
            PT_MVar of int 
          | PT_Root of I.head 
          | PT_Term of I.term 
                       
        fun partial_to_canonical ((PT_Term trm,[]),tp) = trm
          | partial_to_canonical ((ptrm_root,trms),tp) = 
            let 
              val (n, ex_trms) = make_spine tp
              val trms = rev (map (I.weaken_n n) trms) @ ex_trms
              val trm = 
                  case ptrm_root of
                    PT_MVar u => I.MVar(u, rev (map I.M trms))
                  | PT_Root h => I.Root(I.weaken_head n h, trms)
                  | PT_Term t => I.hred(I.weaken_n n t, trms)
              fun add_lambdas 0 trm = trm
                | add_lambdas n trm = I.Lambda'(add_lambdas (n-1) trm)
            in add_lambdas n trm end

        fun app_partial_term (ptrm1,tp1) (ptrm2,tp2) =
            let 
              val trm = partial_to_canonical (ptrm2,tp2)
              val tp = case tp1 of I.Arrow(_,tp) => tp | _ => raise Match
              val ptrm = 
                  case ptrm1 of 
                    (PT_MVar u, trms) => (PT_MVar u, trm :: trms)
                  | (PT_Root h, trms) => (PT_Root h, trm :: trms)
                  | (PT_Term t, trms) => (PT_Term t, trm :: trms)
            in 
              (ptrm, tp)
            end

        fun e2i_neg (trm, vars) = 
            case trm of
              E.Forall(p,tp,x,trm) =>
              let
                val tp = is_groundST tp
                    handle CouldNotInferType
                           => raise ErrPos(p,"Could not infer type of argument")
                val trm = e2i_neg(trm, (V_MVar,x,tp) :: vars)
              in I.Forall(x,trm) end
            | E.Righti(p,trm1,trm2) =>
              let
                val trm1 = e2i_pos(trm1,vars)
                val trm2 = e2i_neg(trm2,vars)
              in I.Righti(trm1,trm2) end
            | E.Lefti(p,trm1,trm2) =>
              let
                val trm1 = e2i_pos(trm1,vars)
                val trm2 = e2i_neg(trm2,vars)
              in I.Lefti(trm1,trm2) end
            | trm => I.Up(e2i_pos (trm,vars))

        and e2i_pos (trm, vars) = 
            case trm of
              E.Fuse(p,trm1,trm2) => 
              let
                val trm1 = e2i_pos(trm1,vars)
                val trm2 = e2i_pos(trm2,vars)
              in I.Fuse(trm1,trm2) end
            | E.Esuf(p,trm1,trm2) => 
              let
                val trm1 = e2i_pos(trm1,vars)
                val trm2 = e2i_pos(trm2,vars)
              in I.Esuf(trm1,trm2) end
            | E.Exists(p,tp,x,trm) =>
              let
                val tp = is_groundST tp
                    handle CouldNotInferType
                           => raise ErrPos(p,"Could not infer type of argument")
                val trm = e2i_pos(trm, (V_MVar,x,tp) :: vars)
              in I.Exists(x,trm) end
            | E.Bang(p,trm) =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(Persistent,a,rev trms)
                | _ => raise Err("Banged not positive proposition\n")
              end
            | E.Gnab(p,trm) =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(Linear,a,rev trms)
                | _ => raise Err("Gnabed not positive proposition\n")
              end
            | E.Not(p,trm) =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.NegAtom(a,rev trms)
                | _ => raise Err("Negated not positive proposition\n")
              end
            | E.One p => I.One
            | trm =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(Ordered,a,rev trms)
                | _ => raise Err("Subterm not positive proposition\n")
              end

        and e2i_term (trm, vars) = 
            case trm of
              E.App(p,trm1,trm2) => 
              let
                val (ptrm1,tp1) = e2i_term(trm1,vars)
                val (ptrm2,tp2) = e2i_term(trm2,vars)
              in app_partial_term (ptrm1,tp1) (ptrm2,tp2) end
            | E.Id(p,[],x) => 
              (let in
                case lookup_var x vars (0,0) of 
                  (* Case 1: Variable in signature *)
                  NONE => ((PT_Root(I.Const x), []), 
                           valOf(MapS.find(signat, x)))
                  (* Case 2: Modal variable *)
                | SOME(V_MVar, u, tp) => ((PT_MVar u, []), tp)
                  (* Case 3: Regular variable *)
                | SOME(V_Var, i, tp) => ((PT_Root(I.Var i), []), tp)
              end
              handle Option => 
                     raise ErrPos(p,"Could not find " ^ x ^ " in signature"))
            | E.Lambda(p,tp1,x,trm) =>
              let 
                val tp1 = is_groundST tp1
                    handle CouldNotInferType
                           => raise ErrPos(p,"Could not infer type of argument")
                val (ptrm,tp2) = e2i_term(trm, (V_Var,x,tp1) :: vars)
                val trm = partial_to_canonical (ptrm,tp2) 
              in ((PT_Term(I.Lambda(x,trm)), []), I.Arrow(tp1,tp2)) end
            | _ => raise ErrPos(E.getpos trm, "Not a term")
            
        fun e2i_decl trm = 
            let in
              case trm of
                E.RULE(p,r,trm) => I.RULE(p,r,e2i_neg(trm,[]))
              | E.EXEC(p,n,trm) => I.EXEC(p,n,e2i_pos(trm,[]))
              | E.TRACE(p,n,trm) => I.TRACE(p,n,e2i_pos(trm,[]))
            end

        val decl = e2i_decl decl

      in decl end

      fun load_decl (signat,extsyn_decl) = 
          let 
            val (new_signat,closed_decl) = infertypes_decl(signat,extsyn_decl)
            val intsyn_decl = transform_decl(new_signat,closed_decl)
          in (new_signat,intsyn_decl) end

  (* Turn all partially instantiated leaves into base type *)

(*
  fun readfile file =
      let 
        val fs = Parse.readfile file
        val (fr,constmap) = reconfile fs
        val signat = MapS.map groundST constmap
        val x = MapS.appi 
                (fn (c,tp) => print(c ^ " : " ^ I.typ_to_string tp ^ ".\n")) 
                signat
        val ft = transformfile (fr, signat)
        val x = List.app (print o I.decl_to_string) ft
      in (ft,signat) end 

  val x = 
   fn () =>
      let in
        print "\n== Combinators ==\n";
        readfile "TEST/comb.olf";
        print "\n== Church numerals ==\n";
        readfile "TEST/church.olf";
        print "\n== Fig. 3: Call-by-value functions ==\n";
        readfile "examples/cbv.olf";
        print "\n== Fig. 4: Mutable storage ==\n";
        readfile "examples/mutable.olf";
        print "\n== Fig. 5: Parallel evaluation for pairs ==\n";
        readfile "examples/pairs.olf";
        print "\n== Fig. 6: Asynchronous communication ==\n";
        readfile "examples/asynch.olf";
        print "\n== Fig. 7: Call-by-name functions ==\n";
        readfile "examples/cbn.olf";
        print "\n== Fig. 8: Call-by-name functions with destinations ==\n";
        readfile "examples/cbn-dest.olf";
        print "\n== Fig. 9: Call-by-need ==\n";
        readfile "examples/cbneed.olf";
        print "\n== Fig. 10: Exceptions ==\n";
        readfile "examples/exceptions.olf";
        ()
      end

  fun readfile file =
      let 
        val fs = Parse.readfile file
        val (fr,constmap) = reconfile fs
        val signat = MapS.map groundST constmap
        val ft = transformfile (fr, signat)
      in (ft,signat) end 
*)

end
