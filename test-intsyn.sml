structure Test = struct
 
  open IntSyn
  val Lambda' = fn trm => Lambda("x",trm)

  fun church_numerals () = 
      let
        val x = print ("== Church numerals ==\n")

        (* λn:(o→o)→o. λf:o→o. λx:o. f (n f x) *)
        val succ = 
            Lambda("n",
              Lambda("f",
                Lambda("x",
                  Var'(1,[Var'(2,[Lambda'(Var'(2,[Var'(0,[])])),
                                  Var'(0,[])])]))))
        val x = print ("+1: " ^ term_to_string succ ^ "\n") 
                
        val plus = 
           Lambda("m",
             Lambda("n",
               Lambda("f",
                 Lambda("x",
                   Var'(3,[Lambda'(Var'(2,[Var'(0,[])])),
                           Var'(2,[Lambda'(Var'(2,[Var'(0,[])])),
                                   Var'(0,[])])])))))
        val x = print ("+: " ^ term_to_string plus ^ "\n") 
                
        (* λf:o→o. λx:o. x *)
        val zero = 
            Lambda("f",Lambda("x",Var'(0,[])))
        val x = print ("0: " ^ term_to_string zero ^ "\n") 
                
        val one = 
            Lambda
              ("f",Lambda
                ("x",hred(succ,[zero,(Lambda'(Var'(2,[Var'(0,[])]))),
                                Var'(0,[])])))
        val x = print ("1: " ^ term_to_string one ^ "\n") 
                
        val two = 
            Lambda
              ("f",Lambda
                ("x",hred(succ,[one,(Lambda'(Var'(2,[Var'(0,[])]))),
                                Var'(0,[])])))
        val x = print ("2: " ^ term_to_string two ^ "\n") 

        val three = 
            Lambda
              ("f",Lambda
                ("x",hred(succ,[two,(Lambda'(Var'(2,[Var'(0,[])]))),
                                Var'(0,[])])))
        val x = print ("3: " ^ term_to_string three ^ "\n") 

        val four = 
            Lambda
              ("f",Lambda
                ("x",hred(succ,[three,(Lambda'(Var'(2,[Var'(0,[])]))),
                                Var'(0,[])])))
        val x = print ("4: " ^ term_to_string four ^ "\n") 
 
        val eight = 
            Lambda
              ("f",Lambda
                ("x",hred(plus,[four,four,
                  (Lambda'(Var'(2,[Var'(0,[])]))),Var'(0,[])])))
        val x = print ("8: " ^ term_to_string eight ^ "\n") 

        val a4 = 
            Lambda("x",
              hred(four,[Lambda'(Const'("suc",[Var'(0,[])])),Var'(0,[])]))
        val x = print ("a4: " ^ term_to_string a4 ^ "\n")

        val b4 = 
            Lambda("f",
              hred(four,[Lambda'(Var'(1,[Var'(0,[])])),Const'("z",[])]))
        val x = print ("b4: " ^ term_to_string b4 ^ "\n")
      in print ("\n") end



  val t0 = Item
  val t1 = Arrow(Item,Item)
  val t01 = Arrow(Item,Arrow(Item,Item))
  val t10 = Arrow(Arrow(Item,Item),Item)
  val t11 = Arrow(Arrow(Item,Item),Arrow(Item,Item))
  val t0_01 = Arrow(Item,Arrow(Item,Arrow(Item,Item)))
  val t0_10 = Arrow(Item,Arrow(Arrow(Item,Item),Item))
  val t0_11 = Arrow(Item,Arrow(Arrow(Item,Item),Arrow(Item,Item)))
  val t1_01 = Arrow(Arrow(Item,Item),Arrow(Item,Arrow(Item,Item)))
  val t1_10 = Arrow(Arrow(Item,Item),Arrow(Arrow(Item,Item),Item))
  val t1_11 = Arrow(Arrow(Item,Item),Arrow(Arrow(Item,Item),Arrow(Item,Item)))
  val t11_11 = Arrow(Arrow(Arrow(Item,Item),Arrow(Item,Item)),
                     Arrow(Arrow(Item,Item),Arrow(Item,Item)))

  fun eta_expansions () = 
      let 
        val f = fn typ => term_to_string (eta_expand_head typ (Const "c"))
      in 
        print ("== Eta Expansions ==\n");
        print (f t0 ^ " : " ^ typ_to_string t0 ^ "\n");
        print (f t01 ^ " : " ^ typ_to_string t01 ^ "\n");
        print (f t10 ^ " : " ^ typ_to_string t10 ^ "\n");
        print (f t11 ^ " : " ^ typ_to_string t11 ^ "\n");
        print (f t11_11 ^ "\n  : " ^ typ_to_string t11_11 ^ "\n");
        print ("\n")
      end

  fun mvars () = 
      let
        val p1 = Const'("lam",[Lambda("x",Lambda("y",MVar(0,[R 0,R 1])))])
        val c1 = Const'("lam",[Lambda("x",Lambda("y",MVar(0,[R 0,R 1])))])
      in
        print ("== MVars and substitutions ==\n");
        print ("p1:  " ^ term_to_string_env ["E"] [] p1 ^ "\n");
        print ("c1:  " ^ term_to_string_env ["E"] [] c1 ^ "\n");
        print ("\n")
      end

  val x = 
      let in
        church_numerals();
        eta_expansions();
        mvars()
      end


(*
  val p2 = Const'M("app",[StoreM(0,[]), StoreM(1,[])])
  val c21 = Const'P("app₁",[EvarP(1,[])])
  val c22 = EvarP(0,[])
  val x = print ("p2: " ^ matchterm_to_string #["E₁","E₂"] [] false p2 ^ "\n")
  val x = print ("c21: " ^ pullterm_to_string #["E₁","E₂"] [] false c21 ^ "\n")
  val x = print ("c22: " ^ pullterm_to_string #["E₁","E₂"] [] false c22 ^ "\n")

  val p31 = Const'M("app₁",[StoreM(0,[])])
  val p32 = StoreM(1,[])
  val c31 = Const'P("app₂",[EvarP(1,[])])
  val c32 = EvarP(0,[])
  val x = print ("p31: " ^ matchterm_to_string #["E₂","V₁"] [] false p31 ^ "\n")
  val x = print ("p32: " ^ matchterm_to_string #["E₂","V₁"] [] false p32 ^ "\n")
  val x = print ("c31: " ^ pullterm_to_string #["E₂","V₁"] [] false c31 ^ "\n")
  val x = print ("c32: " ^ pullterm_to_string #["E₂","V₁"] [] false c32 ^ "\n")

  val p41 = Const'M("app₂",[Const'M("lam",[LambdaM("x",StoreM(0,[true]))])])
  val p42 = StoreM(1,[])
  val c4 = EvarP(0,[EvarP(1,[])])
  val x = print ("p41: " ^ matchterm_to_string #["E₀","V₂"] [] false p41 ^ "\n")
  val x = print ("p42: " ^ matchterm_to_string #["E₀","V₂"] [] false p42 ^ "\n")
  val x = print ("c4:  " ^ pullterm_to_string #["E₀","V₂"] [] false c4 ^ "\n")

  val s0 = Const''("app",[Const''("lam",[Lambdan("x",Var''(0,[]))]),
           Const''("app",[Const''("lam",[Lambdan("y",Var''(0,[]))]),
                         Const''("lam",[Lambdan("z",Const''("c",[]))])])])
  val x = print ("s0: " ^ to_string s0 ^ "\n")
*)

end
