structure Execute = struct

  structure I = IntSyn
  structure T = Term
  open Context
   
  datatype consume = Left | Right
  exception MatchFail
  exception Unimplemented
  exception Invariant


  val gensymb = 
      let val r = ref 0 
      in fn () => (r := !r + 1; "d" ^ Int.toString (!r)) end
 
  (* Pulling ground terms from intsyn terms *)
  fun pull_term evars trm = 
      let in
        case trm of
          I.Lambda(x,trm) => T.Lambdan(x,pull_term evars trm)
        | I.Root(I.Var i,trms) => T.Var'(i,map (pull_term evars) trms)
        | I.Root(I.Const c,trms) => T.Const'(c,map (pull_term evars) trms)
        | I.MVar(u,subst) => 
          T.apply_subst (pull_subst evars subst) (valOf(List.nth(evars,u)))
      end
      (* handle exn => raise exn *)

  and pull_front evars (I.R i) = (T.R i)
    | pull_front evars (I.M trm) = (T.M (pull_term evars trm))
 
  and pull_subst evars subst = map (pull_front evars) subst
      (* handle exn => raise exn *)

  (* Matching ground terms against intsyn terms *)
  fun match_term evars (gtrm,trm) = 
      let in
        case (T.prj gtrm,trm) of
          (T.Lambda(_,gtrm), I.Lambda(_,trm)) => match_term evars (gtrm,trm)
        | (T.Var(i,gtrms), I.Root(I.Var j,trms)) =>
          if i <> j then raise MatchFail 
          else match_terms evars (gtrms,trms)
        | (T.Const(c,gtrms), I.Root(I.Const d,trms)) =>
          if c <> d then raise MatchFail 
          else match_terms evars (gtrms,trms)
        | (_, I.MVar(u,subst)) => 
          let 
            datatype out = 
                     None of (T.term -> T.term option list)
                   | Some of T.term
            fun nth (ev1, NONE :: ev2,0) = 
                None (fn trm => List.revAppend(ev1, SOME trm :: ev2))
              | nth (ev1, SOME trm :: ev2,0) = 
                Some trm
              | nth (ev1, ev :: ev2, n) = nth (ev :: ev1, ev2, n-1)
              | nth _ = raise Subscript
          in
            case nth([],evars,u) of 
              (* XXX Assumes *IDENTITY*, not merely pattern, substitution *)
              None evars => evars gtrm
            | Some gtrm' => 
              if not (T.eq(gtrm,T.apply_subst (pull_subst evars subst) gtrm'))
              then raise MatchFail else evars
          end
        | _ => raise MatchFail
      end
      (* handle exn => raise exn *)

  and match_terms evars ([],[]) = evars
    | match_terms evars (gtrm :: gtrms, trm :: trms) = 
      match_terms (match_term evars (gtrm,trm)) (gtrms,trms)
    | match_terms evars _ = raise ListPair.UnequalLengths


  (* Prefocusing: the get_ordered code internalizes the left-to-right
   * evaluation of ordered propositions and does some fail-fast to prevent
   * focusing phases that can't possibly succeed. *)

  fun get_ordered_atom O (perm,a,trms) =
      let in
        case perm of
          I.Ordered => 
          let
            val (b,gtrms) = hd O
                handle Empty => raise MatchFail
          in 
            if a = b then ([(a,gtrms)], tl O) else raise MatchFail
          end
      end
      (* handle exn => raise exn *)

                                
  fun get_ordered_left OL trm = 
      let in
        case trm of
          I.Exists(_,trm) => get_ordered_left OL trm
        | I.Atom atom => get_ordered_atom OL atom
        | I.Fuse(trm1,trm2) =>
          let 
            val (O2,OL) = get_ordered_left OL trm2
            val (O1,OL) = get_ordered_left OL trm1
          in (O1 @ O2, OL) end
        | I.Esuf(trm1,trm2) => 
          let 
            val (O1,OL) = get_ordered_left OL trm1
            val (O2,OL) = get_ordered_left OL trm2
          in (O1 @ O2, OL) end
      end
      (* handle exn => raise exn *)

  fun get_ordered_right OR trm = 
      case trm of
        I.Exists(_,trm) => get_ordered_right OR trm
      | I.Atom atom => get_ordered_atom OR atom
      | I.Fuse(trm1,trm2) =>
        let 
          val (O1,OR) = get_ordered_right OR trm1
          val (O2,OR) = get_ordered_right OR trm2
        in (O1 @ O2, OR) end
      | I.Esuf(trm1,trm2) => 
        let 
          val (O2,OR) = get_ordered_right OR trm2
          val (O1,OR) = get_ordered_right OR trm1
        in (O1 @ O2, OR) end

  fun get_ordered_neg (OL,OR) trm = 
      let in
        case trm of 
          I.Forall(_,trm) => get_ordered_neg (OL,OR) trm
        | I.Righti(trm1,trm2) =>
          let 
            val (OA,OR) = get_ordered_right OR trm1
            val (OL,OB,OR) = get_ordered_neg (OL,OR) trm2
          in (OL,OA @ OB,OR) end
        | I.Lefti(trm1, trm2) =>
          let 
            val (OA,OL) = get_ordered_left OL trm1
            val (OL,OB,OR) = get_ordered_neg (OL,OR) trm2
          in (OL,OA @ OB,OR) end
        | I.Shift(conc) => (OL,[],OR)
      end
      (* handle exn => raise exn *)
  
  (* Focused proof search *)
 
  (* Left inversion *)
  fun conc_left evars trm = 
      case trm of
        (* XXX CAN ONLY HANDLE NEW CONSTANTS OF BASE TYPE! *)
        (* XXX Not checking for namespace conflicts... *)
        I.Exists(_,trm) => conc_left (SOME(T.Const'(gensymb(),[]))::evars) trm
      | I.Fuse(trm1,trm2) => 
        let 
          val (U1,L1,O1) = conc_left evars trm1
          val (U2,L2,O2) = conc_left evars trm2
        in (U1 @ U2, L1 @ L2, O1 @ O2) end
      | I.Esuf(trm1,trm2) => 
        let 
          val (U1,L1,O1) = conc_left evars trm1
          val (U2,L2,O2) = conc_left evars trm2
        in (U1 @ U2, L1 @ L2, O2 @ O1) end
      | I.Atom(perm,a,trms) =>
        let
          val trms = map (pull_term evars) trms
          val trm = (a,trms)
        in 
          case perm of 
            I.Persistent => ([trm],[],[])
          | I.Linear     => ([],[trm],[])
          | I.Ordered    => ([],[],[trm])
        end


  (* Right focus *)
  fun match_atom (S{persistent,linear,ordered}, evars) (perm,a,trms) = 
      let in
        case perm of
          I.Ordered =>
          let 
            val (a,trms') = hd ordered
            val ctx = 
                S{persistent=persistent, linear=linear, ordered=tl ordered} 
                handle Empty => (print "NOO\n"; raise Empty)
            val evars = match_terms evars (trms',trms)
          in (ctx,evars) end
      end
      (* handle exn => raise exn *)


  fun match_pos (ctx,evars) trm = 
      let in
        case trm of 
          I.Exists(_,trm) => 
          let val (ctx,evars) = match_pos (ctx, NONE :: evars) trm
          in (ctx, tl evars) end
        | I.Fuse(trm1,trm2) =>
          match_pos (match_pos (ctx,evars) trm1) trm2
        | I.Esuf(trm1,trm2) =>
          match_pos (match_pos (ctx,evars) trm1) trm2
        | I.Atom atom => match_atom (ctx,evars) atom
      end
      (* handle exn => raise exn *)

  (* Left focus *)
  fun match_neg (ctx,evars) trm = 
      let in
        case trm of 
          I.Forall(_,trm) => match_neg (ctx,NONE :: evars) trm
        | I.Righti(trm1,trm2) => match_neg (match_pos (ctx,evars) trm1) trm2
        | I.Lefti(trm1,trm2) => match_neg (match_pos (ctx,evars) trm1) trm2
        | I.Shift(conc) => (ctx,evars,conc)
      end
      (* handle exn => raise exn *)

  fun focus (ctx, rules) = 
      let 
        fun focushere (U,L,OL,OR) rule = 
            let 
              val (OL,O,OR) = get_ordered_neg (OL,OR) rule
              val (S{linear=L,...},evars,conc) = 
                  match_neg (S{persistent=U, linear=L, ordered=O}, []) rule
              val (U',L',O') = conc_left evars conc
            in S{persistent = U' @ U,
                 linear = L' @ L, 
                 ordered = rev OL @ O' @ OR} end
            handle MatchFail => 
                   let in
                     case List.getItem OR of
                       NONE => raise MatchFail
                     | SOME(Q, OR) => focushere(U,L,Q::OL,OR) rule
                   end
        fun focusrules (ctx, []) = raise MatchFail
          | focusrules (ctx as S{persistent,linear,ordered}, rule :: rules) =
            focushere (persistent,linear,[],ordered) rule 
            handle MatchFail => focusrules (ctx, rules)
      in
        focusrules(ctx, rules)
      end
      (* handle exn => raise exn *)
  
  fun execfile file = 
      let 
        fun exec trm rules = 
            let 
              val (U,L,O) = conc_left [] trm
              val ctx = S{persistent=U, linear=L, ordered=O}
              fun step ctx = 
                  let in
                    print ("-- " ^ Context.to_string ctx ^ "\n");
                    step(focus(ctx,rules))
                  end
                  handle MatchFail => ()
            in step ctx end

        fun run (decl,rules) =
            let val _ = print (I.decl_to_string decl)
            in
              case decl of
                I.RULE(_,_,rule) => (rules @ [rule])
              | I.EXEC(_,trm) => (exec trm rules; rules)
            end
        val (decls,signat) = TypeRecon.readfile file
        val _ = print "== Implied Signature == \n"
        val _ = TypeRecon.MapS.appi 
                (fn (c,tp) => print(c ^ " : " ^ I.typ_to_string tp ^ ".\n")) 
                signat
        val _ = print "\n== Program == \n"
      in foldl run [] decls; () end

end
