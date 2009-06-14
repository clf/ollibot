structure Frontend = struct

  open Global
  structure I = IntSyn

  fun read file = 
      let 
        fun step (rules, decl) =
            let in
              print (I.decl_to_string decl ^ "\n");
              case decl of 
                I.RULE rule => (rule :: rules)
              | I.EXEC (p,n,pos_prop) => 
                let val (ctx,n) = Execute.execute(pos_prop, rules, n)
                in 
                  print ("-- After " ^ Int.toString n ^ " steps:\n");
                  print ("-- " ^ Context.to_string ctx ^ "\n");
                  rules
                end
              | I.TRACE (p,n_opt,pos_prop) => 
                let fun loop (trace, n, 0) = 
                        print("-- Total of " ^ Int.toString n ^ 
                              " steps (limit reached)\n")
                      | loop (trace, n, m) =
                        case Stream.force trace of
                          Stream.Nil => 
                          print("-- Total of " ^ Int.toString (n-m) ^ 
                                " steps\n")
                        | Stream.Cons(ctx, trace) =>
                          (print ("-- " ^ Context.to_string ctx ^ "\n");
                           loop (trace, n, m-1))
                  val n : int = case n_opt of NONE => 100000 | SOME n => n
                in 
                  loop (Execute.trace(pos_prop, rules),n,n+1);
                  rules
                end
            end
              
        fun loop (signat, rules, tokens) =
            case Parsing.parseWithStream Parse.decl_parser tokens of
              SOME(decl, tokens) =>
              let
                val (signat, decl) = TypeRecon.load_decl (signat, decl)
                val rules = step (rules, decl)
              in loop (signat, rules, tokens) end
            | NONE =>
              (case Stream.force tokens of
                 Stream.Nil => Global.OK
               | Stream.Cons((tok,pos),_) => 
                 raise ErrPos(pos, 
                              "Could not parse declaration beginning with " ^
                              Parse.token_to_string tok))
                 

      in loop (MapS.empty, [], Parse.file_to_tokenstream file) end
      handle Global.Error(NONE,msg) => 
             let in
               print("Error: " ^ file ^ "\n" ^ msg ^ "\n");
               Global.ABORT
             end
           | Global.Error(SOME p,msg) =>
             let in
               print("Error: " ^ Pos.toString p ^ "\n" ^ msg ^ "\n");
               Global.ABORT
             end
           (* | exn =>
             let in
               print ("Unexpected exception " ^ exnName exn);
               print (": " ^ file ^ "\n" ^ exnMessage exn ^ "\n");
               app (fn s => print(s ^ "\n")) (SMLofNJ.exnHistory exn);
               Global.ABORT
             end *)

end
