
structure IntSyn = struct

  datatype perm = Ordered | Linear | Persistent

  datatype tp = Prop | Item | Arrow of tp * tp

  (* A substitution (front list) (Γ ⊢ Δ) substitutes, for each variable in
   * Δ, some term well-defined in Γ. Therefore, it can be seen as a means of
   * transporting terms (Δ ⊢ τ) into terms (Γ ⊢ τ). 
   * 
   * We could have a substitution be just a list of terms; however, it is
   * reasonable to allow eta-contracted terms that do not include lambdas
   * as well. We only allow one particular instance of this, where the
   * term being be substituted can be contracted to a single variable. *)

  datatype front = 
      M of term (* Canonical term *)
    | R of int (* Bare variable (could be generalized) *)

  (* A term (Γ ⊢ τ) must be intrinsically well-typed in the context Γ *)

  and head = Var of int | Const of string

  and term = 
      Lambda of string * term 
    | Root of head * term list
    | MVar of int * front list

  val Lambda' = fn trm => Lambda("x",trm)
  val Var' = fn (i, trms) => Root(Var i, trms)
  val Const' = fn (c, trms) => Root(Const c, trms)

  datatype neg_prop = 
      Forall of string * neg_prop
    | Righti of pos_prop * neg_prop
    | Lefti of pos_prop * neg_prop
    | Shift of pos_prop

  and pos_prop =  
      Exists of string * pos_prop
    | Fuse of pos_prop * pos_prop
    | Esuf of pos_prop * pos_prop
    | Atom of perm * string * term list
   
  type rule = Pos.pos * string * neg_prop
  datatype decl =
      RULE of rule
    | EXEC of Pos.pos * pos_prop

  fun typ_to_string typ = 
      let 
        fun to_string typ needs_parens = 
            case typ of
              Prop => "o"
            | Item => "i"
            | Arrow(t1,t2) => 
              if needs_parens
              then "(" ^ to_string t1 true ^ " → " ^ to_string t2 false ^ ")"
              else to_string t1 true ^ " → " ^ to_string t2 false
      in to_string typ false end

  fun subst_to_string mvars vars subst = 
      let 
        fun front_to_string front = 
            case front of 
              M trm => term_to_string_env mvars vars false trm
            | R i => Names.nth(vars,i)
      in "[" ^ String.concatWith "," (map front_to_string subst) ^ "]" end

  and term_to_string_env mvars vars needs_parens trm = 
      let 
        fun to_string' vars needs_parens trm = 
            case trm of 
              Lambda (x,trm0) => 
              let val x = Names.new_name (mvars @ vars,x) in
                if needs_parens 
                then "(λ" ^ x ^ ". " ^
                     to_string' (x :: vars) false trm0 ^ ")"
                else "λ" ^ x ^ ". " ^ 
                     to_string' (x :: vars) false trm0 
              end
            | MVar(u,[]) => Names.nth(mvars,u)
            | Root(Var j,[]) => Names.nth(vars,j)
            | Root(Const c,[]) => c
            | MVar(u,subst) =>
              Names.nth(mvars,u) ^ subst_to_string mvars vars subst
            | Root(Var j,trms) => 
              if needs_parens
              then
                "(" ^ Names.nth(vars,j) ^ " " ^ args vars trms ^ ")"
              else Names.nth(vars,j) ^ " " ^ args vars trms
            | Root(Const c,trms) => 
              if needs_parens
              then "(" ^ c ^ " " ^ args vars trms ^ ")"
              else c ^ " " ^ args vars trms
        and args vars trms = 
            String.concatWith " " (map (to_string' vars true) trms)
      in to_string' vars needs_parens trm end

  fun pos_prop_to_string_env mvars needs_parens trm = 
      let val to_string = pos_prop_to_string_env
      in
        case trm of 
          Exists (x,trm0) => 
          let val x = Names.new_name (mvars,x) in
            if needs_parens
            then "(∃" ^ x ^ ". " ^
                 to_string (x :: mvars) false trm0 ^ ")"
            else "∃" ^ x ^ ". " ^ 
                 to_string (x :: mvars) false trm0 
          end
        | Fuse(trm1,trm2) =>
          let 
            val str = 
                to_string mvars true trm1 ^ " • " ^
                to_string mvars false trm2
          in if needs_parens then "(" ^ str ^ ")" else str end
        | Esuf(trm1,trm2) =>
          let 
            val str = 
                to_string mvars true trm1 ^ " ○ " ^
                to_string mvars false trm2
          in if needs_parens then "(" ^ str ^ ")" else str end
        | Atom(Persistent,a,trms) => 
          "!" ^ term_to_string_env mvars [] false (Root(Const a,trms))
        | Atom(Linear,a,trms) => 
          "¡" ^ term_to_string_env mvars [] false (Root(Const a,trms))
        | Atom(Ordered,a,trms) => 
          term_to_string_env mvars [] false (Root(Const a,trms))
      end

  fun neg_prop_to_string_env mvars needs_parens trm = 
      let val to_string = neg_prop_to_string_env
        val to_string_pos = pos_prop_to_string_env
      in
        case trm of 
          Forall (x,trm0) => 
          let val x = Names.new_name (mvars,x) in
            if needs_parens
            then "(∀" ^ x ^ ". " ^
                 to_string (x :: mvars) false trm0 ^ ")"
            else "∀" ^ x ^ ". " ^ 
                 to_string (x :: mvars) false trm0 
          end
        | Righti(trm1,trm2) =>
          let 
            val str = 
                to_string_pos mvars false trm1 ^ " ->> " ^ 
                to_string mvars false trm2
          in if needs_parens then "(" ^ str ^ ")" else str end
        | Lefti(trm1,trm2) =>
          let 
            val str = 
                to_string_pos mvars false trm1 ^ " >-> " ^ 
                to_string mvars false trm2
          in if needs_parens then "(" ^ str ^ ")" else str end
        | Shift trm => to_string_pos mvars false trm
      end

  fun decl_to_string (RULE(p,r,trm)) =
      r ^ " : " ^ neg_prop_to_string_env [] false trm ^ ".\n"
    | decl_to_string (EXEC(p,trm)) =
      "%exec " ^ pos_prop_to_string_env [] false trm ^ ".\n"

  val term_to_string = term_to_string_env [] []

  exception HSubst

  (* weaken_head : (Δ:n) → (τ ∈ Γ) → (τ ∈ Γ,σ) *)
  fun weaken_head n (Const c) = Const c
    | weaken_head n (Var i) = Var (i+n)

  (* Given a type, a head, and a partial spine, create a canonical term *)
  (* eta_expand : τ → (σ ∈ Γ) → (Γ [σ] ⊢ τ) → (Γ ⊢ τ) *)
  fun eta_expand typ h trms = 
      let 
        fun args typ i = case typ of Arrow(t1,t2) => args t2 (i+1) | _ => i
        fun spine typ i = 
            case typ of
              Arrow(t1,t2) => eta_expand_head t1 (Var(i-1)) :: spine t2 (i-1) 
            | _ => []
        fun lambdas trm 0 = trm
          | lambdas trm n = Lambda'(lambdas trm (n-1))
        val num_args = args typ 0
        val atomic_trm = Root(weaken_head num_args h, spine typ num_args)
      in lambdas atomic_trm num_args end

  (* Same as above, but no partial spine *)
  (* eta_expand_head : Πτ. (τ ∈ Σ ∪ Γ) → (Γ ⊢ τ) *)
  and eta_expand_head typ h = eta_expand typ h []

  (* weaken: (Γ ⊢ τ) → (Γ,σ ⊢ τ) *)
  fun weaken trm =
      let 
        (* shift_subst: Δ:n → (Γ ⊢ Ψ) → (Γ,σ,Δ ⊢ Ψ *)
        fun shift_front n front = 
            case front of 
              M trm => M (shift n trm)
            | R i => if i < n then R i else R (i+1)

        (* shift: Δ:n → (Γ ⊢ τ) → (Γ,σ,Δ ⊢ τ) *)
        and shift n trm = 
              case trm of
                Lambda(x,trm0) => Lambda(x, shift (n+1) trm0)
              | Root(Var i,trms) => 
                if i < n then Root(Var i, map (shift n) trms)
                else Root(Var(i+1), map (shift n) trms)
              | Root(Const c,trms) => Root(Const c, map (shift n) trms)
              | MVar(u,subst) => MVar(u, map (shift_front n) subst)

      in shift 0 trm end

  (* weaken_subst (Γ ⊢ Ψ) → (Γ,τ ⊢ Ψ) *)
  fun weaken_subst subst = 
      let
        fun weaken_front front = 
            case front of 
              M trm => M (weaken trm)
            | R i => R (i+1)
      in map weaken_front subst end

  fun weaken_n 0 trm = trm
    | weaken_n n trm = weaken_n (n-1) (weaken trm) 

  (* hsubst: (Γ,Δ ⊢ τ) → (Γ,τ,Δ ⊢ σ) → (Γ,Δ ⊢ σ) *)
  fun hsubst (trm,i) trm' = 
      case trm' of
        Lambda(x, trm0) => Lambda(x, hsubst (weaken trm, i+1) trm0)
      | Root(Var j,trms) =>
        if i = j then hred (trm, map (hsubst (trm,i)) trms)
        else if i < j then Root(Var (j-1), map (hsubst (trm,i)) trms)
        else Root(Var j, map (hsubst (trm,i)) trms)
      | Root(Const c,trms) => Root(Const c,map (hsubst (trm,i)) trms)
      | MVar(u, subst) =>
        MVar(u, map (fn (M trm') => M (hsubst (trm,i) trm')
                      | (R j) => 
                        if i = j then M trm else if i < j then R (j-1) else R j)
                    subst)

  (* hred: (Γ ⊢ τ) → (Γ[τ] ⊢ o) → (Γ ⊢ o) *)
  and hred (trm, trms) = 
      case (trm, trms) of 
        (Lambda(_,trm), trm' :: trms) => hred (hsubst (trm',0) trm , trms)
      | (Root(Var i,trms), []) => Root(Var i,trms)
      | (Root(Const c,trms), []) => Root(Const c, trms)
      | (MVar(u,subst),[]) => MVar(u,subst)
      | (trm,trms) =>
        (print (term_to_string false trm); 
         print " @ ("; 
         print (String.concatWith "; " (map (term_to_string false) trms));
         print ")\n"; 
         raise HSubst)

  (* apply_subst: (subst: Γ ⊢ Ψ) → (trm: Ψ ⊢ τ) → (Γ ⊢ τ) *)
  fun apply_subst subst trm = 
      case trm of 
        Lambda(x, trm0) => Lambda(x, apply_subst (R 0 :: subst) trm0) 
      | Root(Var j, trms) => 
        let val trms = map (apply_subst subst) trms in
          case List.nth(subst, j) of
            R i => Root(Var i, trms)
          | M trm => hred (trm,trms)
        end
      | Root(Const c, trms) => Root(Const c, map (apply_subst subst) trms)
      | MVar(u,subst') => MVar(u,compose_subst subst subst')

  (* compose_subst: (subst1: Γ ⊢ Δ) → (subst2: Δ ⊢ Ψ) → (Γ ⊢ Ψ) *)
  and compose_subst subst1 subst2 = 
      let
        fun compose_front front2 = 
            case front2 of
              R i => List.nth(subst1, i)
            | M trm => M (apply_subst subst1 trm)
      in map compose_front subst2 end

end 
