structure Test = struct
 
  open Term

  (* λn:(o→o)→o. λf:o→o. λx:o. f (n f x) *)
  val succ = 
      Lambdan("n",
        Lambdan("f",
          Lambdan("x",
            Var'(1,[Var'(2,[Lambda'(Var'(2,[Var'(0,[])])),Var'(0,[])])]))))
  val x = print ("+1: " ^ to_string succ ^ "\n") 

  val plus = 
      Lambdan("m",
        Lambdan("n",
          Lambdan("f",
            Lambdan("x",
              Var'(3,[Lambda'(Var'(2,[Var'(0,[])])),
                      Var'(2,[Lambda'(Var'(2,[Var'(0,[])])),
                              Var'(0,[])])])))))
  val x = print ("+: " ^ to_string plus ^ "\n") 

  (* λf:o→o. λx:o. x *)
  val zero = 
      Lambdan("f",Lambdan("x",Var'(0,[])))
  val x = print ("0: " ^ to_string zero ^ "\n") 

  val one = 
      Lambdan
        ("f",Lambdan
          ("x",apply(succ,[zero,(Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
  val x = print ("1: " ^ to_string one ^ "\n") 

  val two = 
      Lambdan
        ("f",Lambdan
          ("x",apply(succ,[one,(Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
  val x = print ("2: " ^ to_string two ^ "\n") 

  val three = 
      Lambdan
        ("f",Lambdan
          ("x",apply(succ,[two,(Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
  val x = print ("3: " ^ to_string three ^ "\n") 

  val four = 
      Lambdan
        ("f",Lambdan
          ("x",apply(succ,[three,(Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
  val x = print ("4: " ^ to_string four ^ "\n") 
 
  val eight = 
      Lambdan
        ("f",Lambdan
          ("x",apply(plus,[four,four,
                           (Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
  val x = print ("8: " ^ to_string eight ^ "\n") 

  val a4 = 
      Lambdan("x",
        apply(four,[Lambda'(Const'("suc",[Var'(0,[])])),Var'(0,[])]))
  val x = print ("a4: " ^ to_string a4 ^ "\n")

  val b4 = 
      Lambdan("f",
        apply(four,[Lambda'(Var'(1,[Var'(0,[])])),Const'("z",[])]))
  val x = print ("b4: " ^ to_string b4 ^ "\n")

  open Rule 

  val p1 = ConstM("lam",[LambdaM("x",StoreM(0,[true]))])
  val c1 = ConstP("lam",[LambdaP("x",EvarP(0,[VarP(0,[])]))])
  val x = print ("p1:  " ^ matchterm_to_string #["E"] [] false p1 ^ "\n")
  val x = print ("c1:  " ^ pullterm_to_string #["E"] [] false c1 ^ "\n")
 
  val p2 = ConstM("app",[StoreM(0,[]), StoreM(1,[])])
  val c21 = ConstP("app₁",[EvarP(1,[])])
  val c22 = EvarP(0,[])
  val x = print ("p2: " ^ matchterm_to_string #["E₁","E₂"] [] false p2 ^ "\n")
  val x = print ("c21: " ^ pullterm_to_string #["E₁","E₂"] [] false c21 ^ "\n")
  val x = print ("c22: " ^ pullterm_to_string #["E₁","E₂"] [] false c22 ^ "\n")

  val p31 = ConstM("app₁",[StoreM(0,[])])
  val p32 = StoreM(1,[])
  val c31 = ConstP("app₂",[EvarP(1,[])])
  val c32 = EvarP(0,[])
  val x = print ("p31: " ^ matchterm_to_string #["E₂","V₁"] [] false p31 ^ "\n")
  val x = print ("p32: " ^ matchterm_to_string #["E₂","V₁"] [] false p32 ^ "\n")
  val x = print ("c31: " ^ pullterm_to_string #["E₂","V₁"] [] false c31 ^ "\n")
  val x = print ("c32: " ^ pullterm_to_string #["E₂","V₁"] [] false c32 ^ "\n")

  val p41 = ConstM("app₂",[ConstM("lam",[LambdaM("x",StoreM(0,[true]))])])
  val p42 = StoreM(1,[])
  val c4 = EvarP(0,[EvarP(1,[])])
  val x = print ("p41: " ^ matchterm_to_string #["E₀","V₂"] [] false p41 ^ "\n")
  val x = print ("p42: " ^ matchterm_to_string #["E₀","V₂"] [] false p42 ^ "\n")
  val x = print ("c4:  " ^ pullterm_to_string #["E₀","V₂"] [] false c4 ^ "\n")

  val s0 = Const'("app",[Const'("lam",[Lambdan("x",Var'(0,[]))]),
           Const'("app",[Const'("lam",[Lambdan("y",Var'(0,[]))]),
                         Const'("lam",[Lambdan("z",Const'("c",[]))])])])
  val x = print ("s0: " ^ to_string s0 ^ "\n")
  (* val x = Engine.matchterm(s0,p2)
  val x = print ("s01: " ^ to_string (Engine.pullterm c21) ^ "\n")
  val x = print ("s02: " ^ to_string (Engine.pullterm c22) ^ "\n") *)
  
  val s10 = Const'("app₁",[
                   Const'("app",[Const'("lam",[Lambdan("y",Var'(0,[]))]),
                                 Const'("lam",[Lambdan("z",Const'("c",[]))])])])
  val s11 = Const'("lam",[Lambdan("x",Var'(0,[]))])


  val r1 = (#["E"], 
            [("eval",Ordered,[p1])],
            [("return",Ordered,[c1])])
  val r2 = (#["E₁","E₂"], 
            [("eval",Ordered,[p2])],
            [("comp",Ordered,[c21]),
             ("eval",Ordered,[c22])])
  val r3 = (#["E₂","V₁"], 
            [("comp",Ordered,[p31]),
             ("return",Ordered,[p32])],
            [("comp",Ordered,[c31]),
             ("eval",Ordered,[c32])])
  val r4 = (#["E₀","V₂"], 
            [("comp",Ordered,[p41]),
             ("return",Ordered,[p42])],
            [("eval",Ordered,[c4])])
  val x = print ("r1: " ^ rule_to_string r1 ^ "\n")
  val x = print ("r2: " ^ rule_to_string r2 ^ "\n")
  val x = print ("r3: " ^ rule_to_string r3 ^ "\n")
  val x = print ("r4: " ^ rule_to_string r4 ^ "\n")

  val om0 = Context.S{ordered = [("eval", [s0])]}
  val x = print ("Ω₀: " ^ Context.to_string om0 ^ "\n")
  val om1 = Exec.matchrule(r2,om0)
  val x = print ("Ω₁: " ^ Context.to_string om1 ^ "\n")
  val om2 = Exec.matchrule(r1,om1)
  val x = print ("Ω₂: " ^ Context.to_string om2 ^ "\n")
  val om3 = Exec.matchrule(r3,om2)
  val x = print ("Ω₃: " ^ Context.to_string om3 ^ "\n")
  val om4 = Exec.matchrule(r2,om3)
  val x = print ("Ω₄: " ^ Context.to_string om4 ^ "\n")
  val om5 = Exec.matchrule(r1,om4)
  val x = print ("Ω₅: " ^ Context.to_string om5 ^ "\n")
  val om6 = Exec.matchrule(r3,om5)
  val x = print ("Ω₆: " ^ Context.to_string om6 ^ "\n")
  val om7 = Exec.matchrule(r1,om6)
  val x = print ("Ω₇: " ^ Context.to_string om7 ^ "\n")
  val om8 = Exec.matchrule(r4,om7)
  val x = print ("Ω₈: " ^ Context.to_string om8 ^ "\n")
  val om9 = Exec.matchrule(r1,om8)
  val x = print ("Ω₉: " ^ Context.to_string om9 ^ "\n")
  val om10 = Exec.matchrule(r4,om9)
  val x = print ("Ω₁₀: " ^ Context.to_string om10 ^ "\n")
  val om11 = Exec.matchrule(r1,om10)
  val x = print ("Ω₁₁: " ^ Context.to_string om11 ^ "\n")

  val x = Exec.exec([r1,r2,r3,r4], om0)

end
