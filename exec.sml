
structure Exec = struct

  val x = print "ABC\n" 
  val x = print "λπω\n"
  val x = print "comp(app₂ (lam λx.x)) • eval(app (lam λy.y) (lam λz.c))\n"
          
  structure MapI = 
  RedBlackMapFn(struct type ord_key = int val compare = Int.compare end)
  
  open Rule 
  open Term
          
  exception RuleFail
  exception EquateFail
  exception ContextFail
            
  val regs : term option array = Array.tabulate(100, fn x => NONE)
  val reset = fn () => Array.modifyi (fn (x,_) => NONE) regs
  val printregs = fn () =>
      Array.appi (fn (x,NONE) => ()
                   | (x,SOME trm) =>  print (Int.toString x ^ ": " ^
                                             to_string trm ^ "\n")) regs
  val sub = fn i => valOf (Array.sub(regs,i))
  val update = fn (i,trm) => Array.update(regs,i,SOME trm)

  fun pullterm trm = 
      case trm of 
        LambdaP(x,trm) => Lambdan(x,pullterm trm)
      | VarP(i,trms) => Var'(i, map pullterm trms)
      | ConstP(c,trms) => Const'(c, map pullterm trms)
      | EvarP(x,trms) => subst(sub x, map pullterm trms)

  fun matchterm (trm, mtrm) =
      case (prj trm, mtrm) of
        (_, StoreM(i,_)) => 
        update(i,trm)
      | (_, MatchM(i,trms)) => 
        if eq(trm,subst(sub i,map pullterm trms)) 
        then () else raise EquateFail
      | (Lambda(_,trm), LambdaM(_,mtrm)) => matchterm (trm, mtrm)
      | (Var(i1,trms), VarM(i2,mtrms)) =>
        if i1 <> i2 then raise EquateFail 
        else ListPair.app matchterm (trms,mtrms)
      | (Const(c1,trms), ConstM(c2,mtrms)) =>
        if c1 <> c2 then raise EquateFail 
        else ListPair.app matchterm (trms,mtrms)
      | _ => raise EquateFail
  
  fun matchrule ((_,prems,concs),Context.S{ordered}) = 
      let 
        fun trymatch (leftO,[],rightO) = (* SUCCESS! *)
            let 
              fun mapper(name,Ordered,trms) = (name,map pullterm trms)
              val midO = map mapper concs
            in Context.S{ordered = (rev leftO) @ midO @ rightO} end
          (* Ran outta context *)
          | trymatch(leftO,_,[]) = raise ContextFail 
          (* Make progress *)
          | trymatch(leftO,(name,_,mtrms) :: prems,(name',trms) :: rightO) =
            if name <> name' then raise EquateFail
            else (ListPair.app matchterm (trms,mtrms); 
                  trymatch(leftO,prems,rightO))
        fun tryfoc (leftO,rightO) = 
            trymatch (leftO,prems,rightO)
            handle ContextFail => raise RuleFail
                 | EquateFail => 
                   (case rightO of 
                      [] => raise RuleFail
                    | atom :: rightO => 
                      tryfoc(atom :: leftO,rightO))
      in tryfoc ([],ordered) end

  fun step ([], state) = NONE
    | step (rule :: rules, state) = 
      SOME(matchrule(rule, state)) handle RuleFail => step(rules, state)

  fun exec (rules, state) = 
      let in
        print("-- " ^ Context.to_string state ^ "\n");
        case step(rules, state) of 
          NONE => print "done.\n"
        | SOME state => exec(rules, state)
      end

end
