
structure Names = struct

  fun new_name(vars,x) =
      let 
        fun new_name_backup (vars,x,i) = 
            if List.exists (fn y => (x ^ Int.toString i) = y) vars
            then new_name_backup (vars,x,i+1)
            else x ^ Int.toString i
        fun new_name' (vars,x) =
            if List.exists (fn y => x = y) vars 
            then new_name_backup (vars,x,1) else x
      in case x of 
           NONE => new_name' (vars,"x")
         | SOME x => new_name' (vars,x)
      end

  fun nth (vars,j) = 
      if length vars > j then List.nth(vars,j)
      else "xx" ^ Int.toString(j - length vars)
           
end

signature TERM = sig
                    type term 
                    datatype term_view =
                        Lambda of string option * term
                      | Var of int * term list
                      | Const of string * term list
                    val Lambda' : term -> term
                    val Lambdan : string * term -> term 
                    val Var' : int * term list -> term
                    val Const' : string * term list -> term
                    val prj : term -> term_view
                    val inj : term_view -> term
                    val apply : term * term list -> term
                    val subst : term * term list -> term
                    val to_string : term -> string
                    val to_string_env : string list -> term -> string
                    val eq : term * term -> bool
                  end

structure Term :> TERM = 
struct

  datatype term_view =
      Lambda of string option * term_view
    | Var of int * term_view list
    | Const of string * term_view list
               
  fun to_string_env env trm = 
      let 
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
      in to_string' env false trm end

  val to_string = to_string_env []

  type term = term_view
              
  val prj = fn x => x
  val inj = fn x => x
  val Lambda' = fn trm => Lambda (NONE, trm)
  val Lambdan = fn (x,trm) => Lambda (SOME x, trm)
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
      (print (to_string trm); 
       print " @ ("; 
       print (String.concatWith "; " (map to_string trms)); print ")\n"; 
       raise HSubst)

  datatype tp = Base | Arrow of tp * tp

  val apply = hred

  fun subst (trm,sigma) = 
      let 
        fun subst' n trm =
            case trm of 
              Lambda(x,trm) => Lambda(x,subst' (n+1) trm)
            | Var(i,trms) =>
              if i < n then Var(i,map (subst' n) trms)
              else apply(List.nth(sigma, i-n), trms)
            | Const(c,trms) => Const(c,map (subst' n) trms)
      in subst' 0 trm end

  fun eq (trm1,trm2) = 
      case (trm1,trm2) of
        (Lambda(_,trm1), Lambda(_,trm2)) => eq (trm1, trm2)
      | (Const(s1,trms1), Const(s2,trms2)) => 
        s1 = s2 andalso ListPair.all eq (trms1,trms2)
      | (Var(i1,trms1), Var(i2,trms2)) =>
        i1 = i2 andalso ListPair.all eq (trms1,trms2)
      | _ => false

end

