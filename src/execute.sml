signature EXECUTE = sig

  val trace :
      IntSyn.pos_prop * Signat.state -> Context.context Stream.stream
  val execute : 
      IntSyn.pos_prop * Signat.state * int option -> Context.context * int

end

structure Execute :> EXECUTE = struct

  open Global
  open Stream
  open List
  open Context
  structure I = IntSyn
  structure T = Term
   
  type atom = string * T.term list
  exception MatchFail
  exception Unimplemented
  exception Invariant

  (* == Some preliminary functions == *)

  val gensymb = 
      let val r = ref 0 
      in fn () => (r := !r + 1; "d" ^ Int.toString (!r)) end
 
  val eqatom : atom -> atom -> bool = 
   fn (a,  trms) =>
   fn (a', trms') =>
      a = a' andalso ListPair.all T.eq (trms,trms')

  (* insertNoDup : atom list * atom list -> atom list * bool
   * Takes every item in the second list and inserts it into the first list
   * if it didn't already exist. Returns "true" if there was something new
   * in the second list. *)
  fun insertNoDup (U,newU) = 
      let fun loop U [] notsat = (U,notsat)
            | loop U (atom :: newU) notsat = 
              if List.exists (eqatom atom) U
              then loop U newU notsat
              else loop (atom :: U) newU true
      in loop U newU false end

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
          _ => ([], O)
      (* end
      handle exn => raise exn *)

                                
  fun get_ordered_left OL trm = 
      (* let in *)
        case trm of
          I.Exists(_,trm) => get_ordered_left OL trm
        | I.Atom atom => get_ordered_atom OL atom
        | I.Eq atom => ([], OL)
        | I.Neq atom => ([], OL)
        | I.Unit => ([], OL)
        | I.Conj(trm1,trm2) =>
          let 
            val (O2,OL) = get_ordered_left OL trm2
            val (O1,OL) = get_ordered_left OL trm1
          in (O1 @ O2, OL) end
      (* end
      handle exn => raise exn *)

  fun get_ordered_right OR trm = 
      (* let in *)
        case trm of
          I.Exists(_,trm) => get_ordered_right OR trm
        | I.Atom atom => get_ordered_atom OR atom
        | I.Eq atom => ([], OR)
        | I.Neq atom => ([], OR)
        | I.Unit => ([], OR)
        | I.Conj(trm1,trm2) =>
          let 
            val (O1,OR) = get_ordered_right OR trm1
            val (O2,OR) = get_ordered_right OR trm2
          in (O1 @ O2, OR) end
      (* end
      handle exn => raise exn *)


  fun get_ordered_neg (OL,OR) trm = 
      (* let in *)
        case trm of 
          I.Forall(_,trm) => get_ordered_neg (OL,OR) trm
        | I.Lolli(trm1,trm2) =>
          let 
            val (OA,OR) = get_ordered_right OR trm1
            val (OL,OB,OR) = get_ordered_neg (OL,OR) trm2
          in (OL,OA @ OB,OR) end
        | I.Up(conc) => (OL,[],OR)
      (* end
      handle exn => raise exn *)
  
  fun prefocus (OL,OR) trm = get_ordered_neg (OL,OR) trm

  (* == PART 3: FOCUSED PROOF SEARCH == *)
  (* Traverse rules to match the context and derive conclusions *)
  (* Stateless except for the ans in match_neg that is permitted to store 
   * solutions in order to do find-all-possibilities saturating search. *)
 
  (* Left inversion *)
  fun conc_left evars trm = 
      case trm of
        (* XXX CAN ONLY HANDLE NEW CONSTANTS OF BASE TYPE! *)
        (* XXX Not checking for namespace conflicts... *)
        I.Exists(_,trm) => conc_left (SOME(T.Const'(gensymb(),[]))::evars) trm
      | I.Eq(_,_) => raise Err "Cannot have equalities in the conclusion" 
      | I.Neq(_,_) => raise Err "Cannot have equalities in the conclusion" 
      | I.Unit => ([], [], [])
      | I.Conj(trm1,trm2) => 
        let 
          val (U1,L1,O1) = conc_left evars trm1
          val (U2,L2,O2) = conc_left evars trm2
        in (U1 @ U2, L1 @ L2, O1 @ O2) end
      | I.Atom(perm,a,trms) =>
        let
          val trms = map (pull_term evars) trms
          val trm = (a,trms)
        in 
          case perm of 
            I.Persistent => ([trm],[],[])
          | I.Linear     => ([],[trm],[])
        end


  (* Right focus *)
  fun match_atom (perm,a,trms) cont (ctx as (U,L,O), evars) = 
      let 
        fun ordered _ _ evars = cont((U,L,tl O), evars)
        fun linear L1 L2 evars = cont((U,List.revAppend(L1,L2),O), evars)
        fun persistent _ _ evars = cont((U,L,O), evars)

        fun run cont (didn't_match,might_match) = 
            case might_match of
              [] => raise MatchFail
            | (b, gtrms) :: might_match =>
              let 
                val () = if a <> b then raise MatchFail else ()
                val evars = match_terms evars (gtrms,trms)
              in cont didn't_match might_match evars end 
              handle MatchFail => 
                     run cont ((b, gtrms) :: didn't_match, might_match)
        fun check cont G = run cont ([],G)
      in
        case perm of
          I.Linear => check linear L
        | I.Persistent => check persistent U
      end

  fun match_pos trm cont (state as (ctx,evars)) = 
      let fun pop cont = fn (ctx,evars) => cont (ctx,tl evars)
      in
        case trm of 
          I.Exists(_,trm1) => match_pos trm1 (pop cont) (ctx, NONE :: evars) 
        | I.Unit => cont(ctx,evars)
        | I.Conj(trm1,trm2) => match_pos trm1 (match_pos trm2 cont) state
        | I.Eq(t1,t2) => if T.eq(pull_term evars t1, pull_term evars t2)
                         then cont(ctx,evars) else raise MatchFail
        | I.Neq(t1,t2) => if T.eq(pull_term evars t1, pull_term evars t2)
                         then raise MatchFail else cont(ctx,evars) 
        | I.Atom atom => match_atom atom cont (ctx,evars)
      end

  (* Left focus *)
  fun match_neg trm ans (state as (ctx,evars)) = 
      let in
        case trm of 
          I.Forall(_,trm) => match_neg trm ans (ctx,NONE :: evars) 
        | I.Lolli(trm1,trm2) => match_pos trm1 (match_neg trm2 ans) state
        | I.Up(conc) => ans (ctx,evars,conc)
      end

  fun focus (S{persistent=U,linear=L,ordered=O}, rules) = 
      let 

        (* Try to focus at a particular place on a particular rule *)
        fun focusrule (U,L,OL,OR) neg_prop =
            let 
              val (OL,O,OR) = get_ordered_neg (OL,OR) neg_prop
              val ((U,L,O),evars,conc) = 
                  match_neg neg_prop (fn x => x) ((U,L,O), [])
              val (U',L',O') = conc_left evars conc
            in
              if not(null O)
              then raise Err "Prefocusing error (internal)"
              else SOME(S{persistent = #1 (insertNoDup(U,U')),
                          linear = L' @ L, 
                          ordered = rev OL @ O' @ OR})
            end
            handle MatchFail => NONE
                 | exn => raise Err "Unexpected exception in focus (internal)"

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

  (* == PART 4: SATURATING PROOF SEARCH == *)
  
  fun immediate_consequence (U, rules) = 
      let 
        val answers : atom list list ref = ref []
        fun ans (_,evars,conc) = 
            let val (U,_,_) = conc_left evars conc 
            in answers := U :: !answers; raise MatchFail
            end
        fun loop [] = List.concat (!answers)
          | loop (neg_prop :: rules) = 
            match_neg neg_prop ans ((U,[],[]), [])
            handle MatchFail => loop rules
      in loop rules end

  fun saturate (S{persistent=U,linear=L,ordered=O}, rules) = 
      let
        fun loop U = 
            let 
              val newfacts = immediate_consequence (U, rules)
              val (U',changed) = insertNoDup(U,newfacts)
            in if changed then loop U' else U' end
      in S{persistent=loop U, linear=L, ordered=O} end 

  (* == PART 5: EXECUTION == *)
  fun trace (pos_prop, st : Signat.state) = 
      let
        val saturating_rules = #saturating_rules st
        val linear_rules = #linear_rules st
        fun complete ctx = saturate(ctx,saturating_rules)
        val (U,L,O) = conc_left [] pos_prop
        val ctx = complete(S{persistent=U, linear=L, ordered=O})
        fun stream ctx () =
            case focus(ctx, linear_rules) of
              NONE => Nil
            | SOME ctx' => 
              let val ctx'' = complete ctx' 
              in Cons(ctx'', delay (stream ctx'')) end
      in
        delay(fn () => Cons(ctx, delay(stream ctx)))
      end

  fun execute (pos_prop, st : Signat.state, stop) = 
      let
        val saturating_rules = #saturating_rules st
        fun complete ctx = saturate(ctx,saturating_rules)
        val (U,L,O) = conc_left [] pos_prop
        val ctx = complete(S{persistent=U, linear=L, ordered=O})

        (* Loop until no more steps can be taken *)
        fun loop n ctx = 
            let val ctx = complete ctx in
              case focus(ctx, #linear_rules st) of
                NONE => (ctx,n)
              | SOME ctx' => loop (n+1) ctx'
            end

        (* Loop for a maximum number of steps *)
        fun loopFor m 0 ctx = (complete ctx,m)
          | loopFor m n ctx = 
            let val ctx = complete ctx in
              case focus(ctx, #linear_rules st) of
                NONE => (ctx,m-n)
              | SOME ctx => loopFor m (n-1) ctx
            end
      in
        (case stop of NONE => loop 0 | SOME m => loopFor m m) ctx
      end
            

end
