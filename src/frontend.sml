structure Frontend = struct

  open Global
  structure I = IntSyn

  fun per_exec (p,n_opt,pos_prop) (st : Signat.state) : unit = 
      let val (ctx,n) = Execute.execute(pos_prop, st, n_opt)
      in 
        print ("-- After " ^ Int.toString n ^ " steps:\n");
        print ("-- " ^ Context.to_string ctx ^ "\n")
      end

  fun per_trace (p,n_opt,pos_prop) (st : Signat.state) : unit = 
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
        loop (Execute.trace(pos_prop, st),n,n+1)
      end

(*
         fun step (state : Signat.state, decl) =
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
 *)

  fun read file = 
      let               
        fun loop (st, tokens) =
            case Parsing.parseWithStream Parse.decl_parser tokens of
              SOME(decl, tokens) =>
              let
                (* Perform type reconstruction; record any new constants *)
                val (decl, st) = MorphSignat.recon(decl, st) 

                (* Load declaration into signature *)
                val st = MorphSignat.update (decl, st)

                (* Output new declaration *)
                val () = print (I.decl_to_string decl ^ "\n")

                (* Run any executable content of declaration *)
                val () = case decl of
                           I.EXEC decl => per_exec decl st
                         | I.TRACE decl => per_trace decl st
                         | _ => ()
              in
                (* Read some more tokens *)
                loop (st, tokens)
              end

            | NONE => (* Just check to make sure we've used all tokens *)
              (case Stream.force tokens of
                 Stream.Nil => Global.OK
               | Stream.Cons((tok,pos),_) => 
                 raise ErrPos(pos, 
                              "Could not parse declaration beginning with " ^
                              Parse.token_to_string tok))

      in loop (Signat.empty, Parse.file_to_tokenstream file) end

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
           | exn =>
             let in
               print ("Unexpected exception " ^ exnName exn);
               print (": " ^ file ^ "\n" ^ exnMessage exn ^ "\n");
               app (fn s => print(s ^ "\n")) (SMLofNJ.exnHistory exn);
               Global.ABORT
             end

end
