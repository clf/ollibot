signature MORPH_SIGNAT = sig

  (* Because type declarations are implicit, reconstructing a decl can
   * modify the state (by extending the constant signature) *)
  val recon : ExtSyn.decl * Signat.state -> IntSyn.decl * Signat.state

  (* A decl updates the state *)
  val update : IntSyn.decl * Signat.state -> Signat.state

end

structure MorphSignat :> MORPH_SIGNAT = struct

  open Signat
  open Global
  infix w'constants w'linear_preds w'pers_preds w'rules w'linear_rules w'saturating_rules
  structure I = IntSyn

  fun recon (decl, st : state) = 
    let val (constants',decl') = TypeRecon.load_decl (#constants st, decl)
    in (decl', st w'constants constants') end

  (* Check whether a decl is entirely persistent *)
  fun allpers_neg prop = 
      case prop of 
        I.Forall(_,prop) => allpers_neg prop
      | I.Lolli(prop1,prop2) => allpers_pos prop1 andalso allpers_neg prop2
      | I.Up(prop) => allpers_pos prop
  and allpers_pos prop = 
      case prop of
        I.Exists(_,prop) => allpers_pos prop
      | I.Unit => true
      | I.Conj(prop1,prop2) => allpers_pos prop1 andalso allpers_pos prop2
      | I.Eq(_,_) => true
      | I.Neq(_,_) => true
      | I.Atom(I.Persistent,_,_) => true
      | I.Atom(I.Linear,_,_) => false

  (* Check for range restriction *)
  fun checkstrict_neg prop = ()

  (* Check for consistent use of exponentials *)
  fun checkexp_pos (p,prop) (pers,linear) =
    case prop of
      I.Exists(_,prop) => checkexp_pos (p,prop) (pers,linear)
    | I.Unit => (pers,linear)
    | I.Neq _ => (pers,linear)
    | I.Eq _ => (pers,linear)
    | I.Conj(p1, p2) => checkexp_pos (p,p2) (checkexp_pos (p,p1) (pers,linear))
    | I.Atom(I.Persistent,a,_) =>
      if SetS.member(pers,a) then (pers,linear)
      else if SetS.member(linear,a) 
      then raise ErrPos(p,"Predicate \"" ^ a ^ 
                          "\" has a persistent use, was previously linear.")
      else (SetS.add(pers,a),linear)
    | I.Atom(I.Linear,a,_) =>
      if SetS.member(linear,a) then (pers,linear)
      else if SetS.member(pers,a) 
      then raise ErrPos(p,"Predicate \"" ^ a ^ 
                          "\" has a linear use, was previously persistent.")
      else (pers,SetS.add(linear,a))

  fun checkexp_neg (p,prop) (pers,linear) = 
    case prop of
      I.Forall(_,prop) => checkexp_neg (p,prop) (pers,linear)
    | I.Lolli(p1, p2) => checkexp_neg (p,p2) (checkexp_pos (p,p1) (pers,linear))
    | I.Up(prop) => checkexp_pos (p,prop) (pers,linear)

  fun update (decl, st) = 
    case decl of
      I.RULE (rule as (p,_,prop)) => 
      let
        val (pers,linear) = 
            checkexp_neg (p,prop) (#pers_preds st, #linear_preds st)
        val st = st w'rules (rule :: #rules st)
                    w'pers_preds pers w'linear_preds linear
      in
        checkstrict_neg prop;
        if allpers_neg prop 
        then st w'saturating_rules ((p,prop) :: #saturating_rules st)
        else st w'linear_rules ((p,prop) :: #linear_rules st)
      end
    | I.EXEC (p,_,prop) => 
      let val (pers,linear) = 
              checkexp_pos (p,prop) (#pers_preds st, #linear_preds st)
      in st w'pers_preds pers w'linear_preds linear end
    | I.TRACE (p,_,prop) => 
      let val (pers,linear) = 
              checkexp_pos (p,prop) (#pers_preds st, #linear_preds st)
      in st w'pers_preds pers w'linear_preds linear end
      
end
