structure TypeRecon = struct
  
  structure MapS = 
  RedBlackMapFn(struct type ord_key = string val compare = String.compare end)

  structure ST = SimpleType
  structure E = ExtSyn
  structure I = IntSyn

  (* Type approximation and closure 
   * 
   * In this stage, we determine all types in the file, and discover any
   * type errors. We use a "file-wide" means of estimating the . 
   * 
   *** Types are determined by unification using the SimpleType structure
   *** Free variables are determined by 
   *)
  fun reconfile fs = 
      let 
        val fl = Stream.tolist fs

        (* This map stores the inferred global signature for constants *)
        val constmap : ST.styp MapS.map ref = ref MapS.empty
        fun lookup s = 
            case MapS.find(!constmap,s) of
              NONE => let val t = ST.Var'()
              in constmap := MapS.insert(!constmap,s,t); t end
            | SOME t => t 
        fun unify (x,y) = ST.unify x y

        (* vars_and_types(trm, bvar) = (fvar, tp) 
         ** trm - a term in the external syntax
         ** bvar - a map from bound variables to their inferred types 
         ** fvar - a set of free variables within the term trm
         ** tp - an inferred type for the term trm *)
        fun vars_and_types (trm, bvar) = 
            case trm of 
              E.App(p,trm1,trm2) =>
              let (* val _ = print "App\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
                val (fvar2,tp2) = vars_and_types (trm2, bvar)
                val tp = ST.Var'()
                val fvar = MapS.unionWith unify (fvar1,fvar2)
              in 
                ST.unify tp1 (ST.Arrow'(tp2,tp)); (fvar, tp)
              end
            | E.Forall(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (fvar0,tp0) = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in ST.unify tp0 ST.Prop'; (fvar0, ST.Prop') end
            | E.Exists(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (fvar0,tp0) = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in ST.unify tp0 ST.Prop'; (fvar0, ST.Prop') end
            | E.Fuse(p,trm1,trm2) =>
              let (* val _ = print "Fuse\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
                val (fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = MapS.unionWith unify (fvar1,fvar2)
              in
                unify(tp1, ST.Prop'); unify(tp2, ST.Prop'); (fvar, ST.Prop')
              end
            | E.Esuf(p,trm1,trm2) =>
              let (* val _ = print "Fuse\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
                val (fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = MapS.unionWith unify (fvar1,fvar2)
              in
                unify(tp1, ST.Prop'); unify(tp2, ST.Prop'); (fvar, ST.Prop')
              end
            | E.Righti(p,trm1,trm2) =>
              let (* val _ = print "Righti\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
                val (fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = MapS.unionWith unify (fvar1,fvar2)
              in
                unify(tp1, ST.Prop'); unify(tp2, ST.Prop'); (fvar, ST.Prop')
              end
            | E.Lefti(p,trm1,trm2) =>
              let (* val _ = print "Lefti\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
                val (fvar2,tp2) = vars_and_types (trm2, bvar)
                val fvar = MapS.unionWith unify (fvar1,fvar2)
              in
                unify(tp1, ST.Prop'); unify(tp2, ST.Prop'); (fvar, ST.Prop')
              end
            | E.Lambda(p,tp,x,trm0) =>
              let (* val _ = print "Lambda\n" *)
                val (fvar0,tp0) = vars_and_types (trm0, MapS.insert(bvar,x,tp))
              in (fvar0, ST.Arrow'(tp,tp0)) end
            | E.Id(p,[],x) =>
              let (* val _ = print ("Id " ^ x ^ "\n") *) in
                case MapS.find(bvar, x) of
                  SOME tp => (* Bound variable, obtain from bvars *)
                  (MapS.empty, tp)
                | NONE =>
                  if Char.isUpper(String.sub(x,0)) 
                  then (* Free variable, bind locally *)
                    let val tp = ST.Var'() 
                    in (MapS.singleton(x,tp), tp) end
                  else (* Constant, get type globally *)
                    let val tp = lookup x
                    in (MapS.empty, tp) end
              end
            | E.Id(p,_,x) => (print "No paths yet!"; raise Match)
            | E.Bang(p,trm1) =>
              let (* val _ = print "Bang\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
              in
                unify(tp1, ST.Prop'); (fvar1, ST.Prop')
              end
            | E.Gnab(p,trm1) =>
              let (* val _ = print "Gnab\n" *)
                val (fvar1,tp1) = vars_and_types (trm1, bvar)
              in
                unify(tp1, ST.Prop'); (fvar1, ST.Prop')
              end


        (* Print a partially inferred type *)
        fun strST tp needs_parens = 
            case ST.prj tp of
              ST.Var ev => "#"
            | ST.Item => "i"
            | ST.Prop => "o"
            | ST.Arrow(t1,t2) =>
              let val s = strST t1 true ^ " → " ^ strST t2 false
              in if needs_parens then "(" ^ s ^ ")" else s end 
                                                         
        fun learntypes (E.RULE(p,s,trm)) = 
            let val (fv, tp) = vars_and_types(trm,MapS.empty)
              fun wrap (fv, tp, trm) = E.Forall(p,tp,fv,trm)
            in
              ST.unify tp ST.Prop'; 
              E.RULE(p,s,MapS.foldri wrap trm fv)
            end
          | learntypes (E.EXEC(p,trm)) = 
            let val (fv, tp) = vars_and_types(trm,MapS.empty)
            in 
              ST.unify tp ST.Prop'; 
              if MapS.isEmpty fv then E.EXEC(p,trm)
              else raise Match (* XXX needs error message *)
            end

        val fr = map learntypes fl

      in (fr,!constmap) end
 
  fun transformfile (fr,constmap) = 
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
        fun is_groundST tp = 
            case ST.prj tp of
              ST.Var ev => (print "Could not infer type\n"; raise Match)
            | ST.Arrow(tp1,tp2) => I.Arrow(is_groundST tp1, is_groundST tp2)
            | ST.Item => I.Item
            | ST.Prop => I.Prop

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
            | trm => I.Shift(e2i_pos (trm,vars))

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
                val trm = e2i_pos(trm, (V_MVar,x,tp) :: vars)
              in I.Exists(x,trm) end
            | E.Bang(p,trm) =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(I.Persistent,a,trms)
                | _ => (print "Banged not positive proposition\n"; raise Match)
              end
            | E.Gnab(p,trm) =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(I.Linear,a,trms)
                | _ => (print "Gnabed not positive proposition\n"; raise Match)
              end
            | trm =>
              let in
                case e2i_term (trm, vars) of
                  ((PT_Root(I.Const a),trms), I.Prop) => 
                  I.Atom(I.Ordered,a,trms)
                | _ => (print "Subterm not positive proposition\n"; raise Match)
              end

        and e2i_term (trm, vars) = 
            case trm of
              E.App(p,trm1,trm2) => 
              let
                val (ptrm1,tp1) = e2i_term(trm1,vars)
                val (ptrm2,tp2) = e2i_term(trm2,vars)
              in app_partial_term (ptrm1,tp1) (ptrm2,tp2) end
            | E.Id(p,[],x) => 
              let in
                case lookup_var x vars (0,0) of 
                  (* Case 1: Variable in signature *)
                  NONE => ((PT_Root(I.Const x), []), MapS.lookup(constmap, x))
                  (* Case 2: Modal variable *)
                | SOME(V_MVar, u, tp) => ((PT_MVar u, []), tp)
                  (* Case 3: Regular variable *)
                | SOME(V_Var, i, tp) => ((PT_Root(I.Var i), []), tp)
              end
            | E.Lambda(p,tp1,x,trm) =>
              let 
                val tp1 = is_groundST tp1
                val (ptrm,tp2) = e2i_term(trm, (V_Var,x,tp1) :: vars)
                val trm = partial_to_canonical (ptrm,tp2) 
              in ((PT_Term(I.Lambda(x,trm)), []), I.Arrow(tp1,tp2)) end
            | _ =>
              (print("Not a term: " ^ Pos.toString (E.getpos trm) ^ "\n");
               raise Match)
            
        fun e2i_decl trm = 
            let in
              case trm of
                E.RULE(p,r,trm) => I.RULE(p,r,e2i_neg(trm,[]))
              | E.EXEC(p,trm) => I.EXEC(p,e2i_pos(trm,[]))
            end

      in map e2i_decl fr end

  (* Turn all partially instantiated leaves into base type *)
  fun groundST tp = 
      case ST.prj tp of
        ST.Var ev => (ST.bind ev ST.Item'; I.Item)
      | ST.Arrow(t1,t2) => I.Arrow(groundST t1, groundST t2)
      | ST.Item => I.Item
      | ST.Prop => I.Prop  

  fun readfile file =
      let 
        val fs = Parse.readfile file
        val (fr,constmap) = reconfile fs
        val signat = MapS.map groundST constmap
        val x = MapS.appi 
                (fn (c,tp) => print(c ^ " : " ^ I.typ_to_string tp ^ ".\n")) 
                signat
        val ft = transformfile (fr, signat)
        val x = List.app I.decl_to_string ft
      in () end 

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

end
