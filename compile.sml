structure Compile = struct

  structure R = Rule
  structure I = IntSyn
  structure T = Term
  open Context

  fun checkvars (0,false::usedvars) = (length usedvars, false, true::usedvars)
    | checkvars (0,true::usedvars) = (length usedvars, true, true::usedvars)
    | checkvars (i,b::usedvars) =
      let val (n,viewed,usedvars) = checkvars(i-1,usedvars)
      in (n,viewed,b::usedvars) end
   
  fun matches_term (usedvars,vars,trm) = 
      case trm of 
        I.Lambda(_,trm) => R.LambdaM(matches_term(usedvars,vars+1,trm))
      | I.Root(I.Var i,trms) => 
        let val (usedvars, trms) = matches_trms(usedvars,vars,trms,[])
        in (usedvars, R.VarP(i,trms)) end
      | I.Root(I.Const c,trms) => 
        let val (usedvars, trms) = matches_trms(usedvars,vars,trms,[])
        in (usedvars, R.ConstP(c,trms)) end
      | I.MVar(u,subst) => 
        let
          val (n,viewed,usedvars) = checkvars(u,usedvars)
          val trm = 
              if not viewed then R.StoreM(i, map (fn _ => true) subst)
              else R.MatchM
        in 
          if viewed 
          then R.Store

  and matches_terms (usedvars,vars,[],out_trms) = (usedvars, rev out_trms)
    | matches_terms (usedvars,vars,trm :: trms,out_trms) =
      let val (usedvars,trm) = matches_trm (usedvars,vars,trm)
      in matches_terms (usedvars,vars,trms,trm :: out_trms) end

  fun matches_neg (usedvars,neg) = 
      case neg of
        I.Forall(_,neg) => matches_neg(false :: usedvars, neg)
      | I.Righti(pos,neg) => 
        let 
          val prems1 = matches_pos (i,pos) 
          val (prems2,conc) = matches_neg (i,neg)
        in (prems1 @ prems2, conc) end
      | I.Shift(conc) => ([], (i, conc))
                         
  and matches_pos (usedvars,pos) = 
      case pos of 
        I.Exists(_,pos) => matches_pos(false :: usedvars, pos)
      | I.Fuse(pos1,pos2) => 
        let 
          val (usedvars,prems1) = matches_pos(usedvars,pos1)
          val (usedvars,prems2) = matches_pos(usedvars,pos2)
        in (usedvars, prems1 @ prems2) end
      | I.Atom(perm,a,trms) =>

end
