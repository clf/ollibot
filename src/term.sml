
signature TERM = 
sig
  type term 
  datatype front = M of term | R of int
  type subst = front list
  datatype term_view =
           Lambda of string * term
         | Var of int * term list
         | Const of string * term list
  val Lambda' : term -> term
  val Lambdan : string * term -> term 
  val Var' : int * term list -> term
  val Const' : string * term list -> term
  val prj : term -> term_view
  val inj : term_view -> term
  val apply : term * term list -> term
  val apply_subst : front list -> term -> term
  val to_string : term -> string
  val to_string_parens : term -> string
  val to_string_env : string list -> term -> string
  val eq : term * term -> bool
end

structure Term :> TERM = 
struct

  open Global

  datatype term_view =
      Lambda of string * term_view
    | Var of int * term_view list
    | Const of string * term_view list
               
  fun to_string' vars needs_parens trm = 
      case trm of 
        Lambda (x,trm0) => 
        let val x = Names.new_name (vars,x) in
          if needs_parens 
          then "(λ" ^ x ^ ". " ^
               to_string' (x :: vars) false trm0 ^ ")"
          else "λ" ^ x ^ ". " ^ 
                     to_string' (x :: vars) false trm0 
        end
      | Var(j,[]) => Names.nth(vars,j)
      | Const(c,[]) => c
      | Var(j,trms) => 
        if needs_parens
        then
          "(" ^ Names.nth(vars,j) ^ " " ^ args vars trms ^ ")"
        else Names.nth(vars,j) ^ " " ^ args vars trms
      | Const(c,trms) => 
        if needs_parens
        then "(" ^ c ^ " " ^ args vars trms ^ ")"
        else c ^ " " ^ args vars trms
  and args vars trms = 
      String.concatWith " " (map (to_string' vars true) trms)


  fun to_string_env vars trm = to_string' vars false trm 

  fun to_string_parens trm = to_string' [] true trm 
  fun to_string trm = to_string' [] false trm

  type term = term_view
  datatype front = M of term | R of int
  type subst = front list
              
  val prj = fn x => x
  val inj = fn x => x
  val Lambda' = fn trm => Lambda ("x", trm)
  val Lambdan = fn (x,trm) => Lambda (x, trm)
  val Var' = Var
  val Const' = Const
                
  (* weaken: (Γ ⊢ τ) → (Γ,σ ⊢ τ) *)
  fun weaken trm =
      (* shift: Δ:n → (Γ ⊢ τ) → (Γ,σ,Δ ⊢ τ) *)
      let fun shift n trm = 
              case trm of
                Lambda(x,trm0) => Lambda(x, shift (n+1) trm0)
              | Var(i,trms) => 
                if i < n then Var(i, map (shift n) trms)
                else Var(i+1, map (shift n) trms)
              | Const(c,trms) => Const(c, map (shift n) trms)
      in shift 0 trm end


  exception HSubst

  (* hsubst: (Γ,Δ ⊢ τ) → (Γ,τ,Δ ⊢ σ) → (Γ,Δ ⊢ σ) *)
  fun hsubst (trm,i) trm' = 
      case trm' of
        Lambda(x, trm0) => Lambda(x, hsubst (weaken trm, i+1) trm0)
      | Var(j,trms) =>
        if i = j then hred (trm, map (hsubst (trm,i)) trms)
        else if i < j then Var(j-1, map (hsubst (trm,i)) trms)
        else Var(j, map (hsubst (trm,i)) trms)
      | Const(c,trms) => Const(c,map (hsubst (trm,i)) trms)

  (* hred: (Γ ⊢ τ) → (Γ[τ] ⊢ o) → (Γ ⊢ o) *)
  and hred (Lambda(_,trm), trm' :: trms) = hred (hsubst (trm',0) trm , trms)
    | hred (Var(i,trms), []) = Var(i,trms)
    | hred (Const(s,trms), []) = Const(s, trms)
    | hred (trm,trms) = 
      raise Err("HSubst: " ^ to_string trm ^ " @ (" ^
                String.concatWith "; " (map to_string trms))

  datatype tp = Base | Arrow of tp * tp

  val apply = hred

  fun weaken_subst [] = []
    | weaken_subst (R i :: subst) = (R (i+1) :: weaken_subst subst)
    | weaken_subst (M trm :: subst) = (M (weaken trm) :: weaken_subst subst)

  fun apply_subst subst trm = 
      case trm of 
        Lambda(x,trm) => Lambda(x, apply_subst (R 0 :: weaken_subst subst) trm)
      | Var(i,trms) => 
        let in
          case List.nth(subst, i) of
            R i => Var(i, map (apply_subst subst) trms)
          | M trm => apply(trm, map (apply_subst subst) trms)
        end
      | Const(c,trms) => Const(c, map (apply_subst subst) trms)

  fun eq (trm1,trm2) = 
      case (trm1,trm2) of
        (Lambda(_,trm1), Lambda(_,trm2)) => eq (trm1, trm2)
      | (Const(s1,trms1), Const(s2,trms2)) => 
        s1 = s2 andalso ListPair.all eq (trms1,trms2)
      | (Var(i1,trms1), Var(i2,trms2)) =>
        i1 = i2 andalso ListPair.all eq (trms1,trms2)
      | _ => false

end

