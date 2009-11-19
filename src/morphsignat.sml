signature MORPH_SIGNAT = sig

  (* Because type declarations are implicit, reconstructing a decl can
   * modify the state (by extending the constant signature) *)
  val recon : ExtSyn.decl * Signat.state -> IntSyn.decl * Signat.state

  (* A decl updates the state *)
  val update : IntSyn.decl * Signat.state -> Signat.state

end

structure MorphSignat :> MORPH_SIGNAT = struct

  open Signat
  infix w'constants w'rules w'linear_rules w'saturating_rules
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

  fun update (decl, st) = 
    case decl of
      I.RULE (rule as (_,_,prop)) => 
      let val st = st w'rules (rule :: #rules st) in
        checkstrict_neg prop;
        if allpers_neg prop 
        then st w'saturating_rules (prop :: #saturating_rules st)
        else st w'linear_rules (prop :: #linear_rules st)
      end
    | I.EXEC _ => st
    | I.TRACE _ => st
      
end
