signature EXECUTE = sig

  val trace :
      IntSyn.pos_prop * IntSyn.rule list -> Context.context Stream.stream
  val execute : 
      IntSyn.pos_prop * IntSyn.rule list * int option -> Context.context * int

end

structure Execute :> EXECUTE = struct

  open Global
  open Stream
  open List
  open Context
  structure I = IntSyn
  structure T = Term
   
  datatype consume = Left | Right
  exception MatchFail
  exception Unimplemented
  exception Invariant


  val gensymb = 
      let val r = ref 0 
      in fn () => (r := !r + 1; "d" ^ Int.toString (!r)) end
 
  (* == PART 1: TERM MATCHING == *)
  (* Currently done very simplisticly, needs to be extended 
   * at least to handle a non-identity pattern substitution
   * as the first evar occurance, and probably to be able to delay
   * until the first strict occurance *)

  (* Pulling ground terms from intsyn terms *)
  fun pull_term evars trm = 
      (* let in *)
        case trm of
          I.Lambda(x,trm) => T.Lambdan(x,pull_term evars trm)
        | I.Root(I.Var i,trms) => T.Var'(i,map (pull_term evars) trms)
        | I.Root(I.Const c,trms) => T.Const'(c,map (pull_term evars) trms)
        | I.MVar(u,subst) => 
          let
            val arg = valOf(List.nth(evars,u))
                handle Option => 
                       raise Err ("Evar " ^ Int.toString u ^ " not ground!")
          in
            T.apply_subst (pull_subst evars subst) (valOf(List.nth(evars,u)))
          end
      (* end
      handle exn => raise exn *)

  and pull_front evars (I.R i) = (T.R i)
    | pull_front evars (I.M trm) = (T.M (pull_term evars trm))
 
  and pull_subst evars subst = map (pull_front evars) subst
      (* handle exn => raise exn *)

  (* Matching ground terms against intsyn terms *)
  fun match_term evars (gtrm,trm) = 
      (* let in *)
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
      (* end
      handle exn => raise exn *)

  and match_terms evars ([],[]) = evars
    | match_terms evars (gtrm :: gtrms, trm :: trms) = 
      match_terms (match_term evars (gtrm,trm)) (gtrms,trms)
    | match_terms evars _ = raise ListPair.UnequalLengths


  (* == PART 2: Ordered Prefocusing == *)
  (* This code internalizes the left-to-right evaluation of ordered
   * propositions. 
   * 
   * The only essential requirement of prefocusing is that it handle the 
   * "resource management" problem, carving out the correct atomic propositions
   * from the context and putting them in a list in the order that they will
   * be needed in the left-to-right traversal of the term.
   *
   * Note that this is NOT the same thing as the order they appear in the 
   * context; if the rule is ((a • b) ◦ (c ◦ d) ->> ...) and we are prefocusing
   * at the point Ω₁[]dcabΩ₂, which means we will suceed, the returned list
   * will be [a,b,c,d].
   * 
   * Prefocusing must return NONE if there is not enough ordered context
   * to allow focusing at a particular point; we additionally fail if
   * it is obvious based only on the predicates that we will fail (i.e. if 
   * need an eval(E) but there is a comp(F) there). *)

  fun get_ordered_atom O (perm,a:string,trms) =
      (* let in *)
        case perm of
          I.Ordered => 
          let
            val (b,gtrms) = hd O
                handle Empty => raise MatchFail
          in 
            if a = b then ([(a,gtrms)], tl O) else raise MatchFail
          end
        | _ => ([], O)
      (* end
      handle exn => raise exn *)

                                
  fun get_ordered_left OL trm = 
      (* let in *)
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
      (* end
      handle exn => raise exn *)

  fun get_ordered_right OR trm = 
      (* let in *)
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
      (* end
      handle exn => raise exn *)


  fun get_ordered_neg (OL,OR) trm = 
      (* let in *)
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
        | I.Up(conc) => (OL,[],OR)
      (* end
      handle exn => raise exn *)
  
  fun prefocus (OL,OR) trm = get_ordered_neg (OL,OR) trm

  (* == PART 3: FOCUSED PROOF SEARCH == *)
  (* Traverse rules to match the context and derive conclusions *)
 
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
  fun match_atom (ctx as (U,L,O), evars) (perm,a,trms) = 
      let 
        fun seek (nomatch,[]) _ = raise MatchFail
          | seek (nomatch, (b,gtrms) :: unknownmatch) (a:string,trms) =
            let
              val () = if a <> b then raise MatchFail else ()
              val evars = match_terms evars (gtrms,trms)
            in
              (List.revAppend(nomatch,unknownmatch), evars)
            end
            handle MatchFail => 
                   seek ((b,gtrms) :: nomatch, unknownmatch) (a,trms)
      in
        case perm of
          I.Ordered =>
          let 
            val (a,trms') = hd O
            val O = tl O
            val evars = match_terms evars (trms', trms)
          in ((U,L,O),evars) end
        | I.Linear => 
          let
            val (L, evars) = seek ([],L) (a,trms)
          in ((U,L,O),evars) end
        | I.Persistent => 
          let
            val (_, evars) = seek([],U) (a,trms)
          in (ctx,evars) end
      end
      (* handle exn => raise exn *)


  fun match_pos trm (ctx,evars) cont = 
      let in
        case trm of 
          I.Exists(_,trm) => 
          let val (ctx,evars) = match_pos (ctx, NONE :: evars) trm
          in cont (ctx, tl evars) end
        | I.Fuse(trm1,trm2) =>
          match_pos trm1 (ctx,evars) (match_pos trm2)
        | I.Esuf(trm1,trm2) =>
          match_pos trm1 (ctx,evars) (match_pos trm2)
        | I.Atom atom => match_atom (ctx,evars) atom cont
      end
      (* handle exn => raise exn *)

  (* Left focus *)
  fun match_neg trm (ctx,evars) = 
      let in
        case trm of 
          I.Forall(_,trm) => match_neg trm (ctx,NONE :: evars) 
        | I.Righti(trm1,trm2) => match_pos (ctx,evars) trm1 (match_neg trm2)
        | I.Lefti(trm1,trm2) => match_pos (ctx,evars) trm1) (match_neg trm2)
        | I.Up(conc) => (ctx,evars,conc)
      end
      (* handle exn => raise exn *)

  fun focus2 (S{persistent=U,linear=L,ordered=O}, rules) = 
      let
        fun focusrule (

  fun focus (S{persistent=U,linear=L,ordered=O}, rules) = 
      let 

        (* Try to focus at a particular place on a particular rule *)
        fun focusrule (U,L,OL,OR) (p,r,neg_prop) =
            let 
              val (OL,O,OR) = get_ordered_neg (OL,OR) neg_prop
              val ((U,L,O),evars,conc) = 
                  match_neg ((U,L,O), []) neg_prop
              val (U',L',O') = conc_left evars conc
            in
              if not(null O)
              then raise ErrPos(p, "Prefocusing error (internal), rule " ^ r)
              else SOME(S{persistent = U' @ U,
                          linear = L' @ L, 
                          ordered = rev OL @ O' @ OR})
            end
            handle MatchFail => NONE
                 | Option =>
                   raise ErrPos(p,"OPTION!!!")

        (* Try to focus at a particular place on any rule *)
        fun focuspos (U,L,OL,OR) rules = 
            case ListUtil.findpartial (focusrule (U,L,OL,OR)) rules of
              NONE => 
              if null OR then NONE 
              else focuspos (U,L,hd OR :: OL, tl OR) rules
            | SOME ctx => SOME ctx

      in
        focuspos (U,L,[],O) rules
      end

  fun trace (pos_prop, rules) = 
      let
        val (U,L,O) = conc_left [] pos_prop
        val ctx = S{persistent=U, linear=L, ordered=O}
        fun stream ctx () =
            case focus(ctx, rules) of
              NONE => Nil
            | SOME ctx' => Cons(ctx', delay (stream ctx'))
      in
        delay(fn () => Cons(ctx, delay(stream ctx)))
      end

  fun execute (pos_prop, rules, stop) = 
      let
        val (U,L,O) = conc_left [] pos_prop
        val ctx = S{persistent=U, linear=L, ordered=O}

        (* Loop until no more steps can be taken *)
        fun loop n ctx = 
            case focus(ctx, rules) of
              NONE => (ctx,n)
            | SOME ctx => loop (n+1) ctx

        (* Loop for a maximum number of steps *)
        fun loopFor m 0 ctx = (ctx,m)
          | loopFor m n ctx = 
            case focus(ctx, rules) of
              NONE => (ctx,m-n)
            | SOME ctx => loopFor m (n-1) ctx
      in
        (case stop of NONE => loop 0 | SOME m => loopFor m m) ctx
      end
            

end
