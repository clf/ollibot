structure Parse = struct

  open Parsing
  
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 ||

  datatype token = 
      PERIOD | LPAREN | RPAREN | FUSE 
    | RIGHTI | LEFTI | BANG | GNAB
    | LAMBDA of string | FORALL of string | EXISTS of string 
    | ID of string list * string 
    | PERCENT of string | COLON | WS

  fun token_to_string token = 
      case token of 
        PERIOD => "." | LPAREN => "(" | RPAREN => ")" | FUSE => "•"
      | RIGHTI => "->>" | LEFTI => ">->" | BANG => "!" | GNAB => "¡"
      | LAMBDA x => "λ " ^ x | FORALL x => "∀ " ^ x | EXISTS x => "∃ " ^ x
      | ID(path,x) => concat (map (fn x => x ^  ".") path) ^ x 
      | PERCENT x => "% " ^ x | COLON => ":" | WS => ""

  val transform_tok1 = 
      let
        val sep = 
            fn "." => false | "(" => false | ")" => false | "•" => false 
             | "!" => false | "¡" => false
             | "λ" => false | "∀" => false | "∃" => false 
             | "\\" => false | "%" => false | ":" => false
             | " " => false | "\t" => false | "\n" => false
             | _ => true
        val ws = 
            fn " " => true | "\t" => true | "\n" => true | _ => false
        val idpart = repeat1 (satisfy sep) wth concat
        val linecomment = 
            literal "%" >> literal "%" >> 
            repeat (satisfy (fn "\n" => false | _ => true)) >> literal "\n" >>
            succeed " "
        val tokenparser = 
            alt [linecomment, 
                 literal ".", literal "(", literal ")", literal "•",
                 literal "!", literal "¡",
                 literal "λ", literal "∀", literal "∃", 
                 literal "\\", literal "%", literal ":",
                 repeat1 (satisfy ws) >> succeed " ", idpart]
      in transform (!! tokenparser) end

  val transform_tok2 = 
      let
        val sep = 
            fn "." => false | "(" => false | ")" => false | "•" => false 
             | "!" => false | "¡" => false
             | "\\" => false | "%" => false | ":" => false
             | " " => false | _ => true
        fun not str pos =
            (print("Error: " ^ Pos.toString pos ^ "\n");
             print("Binder " ^ str ^ " not followed by identifier + period\n");
             raise Match)
        fun backslash pos =
            (print("Error: " ^ Pos.toString pos ^ "\n");
             print("Backslash not permitted\n");
             raise Match)
        val white = repeat (literal " ") >> succeed ()
        val tokenparser = 
            alt [literal "." >> succeed PERIOD,
                 literal "(" >> succeed LPAREN,
                 literal ")" >> succeed RPAREN,
                 literal "•" >> succeed FUSE,
                 literal "!" >> succeed BANG,
                 literal "¡" >> succeed GNAB,
                 literal "%" >> any wth PERCENT,
                 literal ":" >> succeed COLON,
                 literal " " >> succeed WS,
                 literal "->>" >> succeed RIGHTI,
                 literal ">->" >> succeed LEFTI,
                 literal "λ" >> (white >> any << white << literal "." 
                                       ## (fn pos => not "λ" pos)) wth LAMBDA,
                 literal "∀" >> (white >> any << white << literal "." 
                                       ## (fn pos => not "∀" pos)) wth FORALL,
                 literal "∃" >> (white >> any << white << literal "." 
                                       ## (fn pos => not "∃" pos)) wth EXISTS,
                 (!! (literal "\\") -- (fn (_,pos) => backslash pos)), 
                 any wth (fn x => ID([],x))] 
      in transform (!! tokenparser) end 

  val transform_tok3 : 
      (token * Pos.pos) Stream.stream -> ((Pos.pos * token) * Pos.pos) Stream.stream = 
      let 
        fun filter s () = 
            case Stream.force s of
              Stream.Nil => Stream.Nil
            | Stream.Cons((WS,_),s) => filter s ()
            | Stream.Cons((tok,pos),s) => 
              Stream.Cons (((pos,tok),pos), Stream.delay (filter s))
      in fn s => Stream.delay (filter s) end

  val transform_decl =
      let 
        fun fuse pos ((pos1,trm1),(pos2,trm2)) =
            let val pos = Pos.union(Pos.union(pos,pos1),pos2)
            in (pos,ExtSyn.Fuse(pos,trm1,trm2)) end
        fun bang pos ((pos1,trm1)) = 
            let val pos = Pos.union(pos,pos1)
            in (pos,ExtSyn.Bang(pos,trm1)) end
        fun gnab pos ((pos1,trm1)) = 
            let val pos = Pos.union(pos,pos1)
            in (pos,ExtSyn.Gnab(pos,trm1)) end
        fun righti pos ((pos1,trm1),(pos2,trm2)) =
            let val pos = Pos.union(Pos.union(pos,pos1),pos2)
            in (pos,ExtSyn.Righti(pos,trm1,trm2)) end
        fun lefti pos ((pos1,trm1),(pos2,trm2)) =
            let val pos = Pos.union(Pos.union(pos,pos1),pos2)
            in (pos,ExtSyn.Lefti(pos,trm1,trm2)) end
        fun lambda ((pos,x),(pos',trm)) = 
            let val pos = Pos.union(pos,pos') 
            in (pos,ExtSyn.Lambda(pos,SimpleType.Var'(),x,trm)) end
        fun forall ((pos,x),(pos',trm)) = 
            let val pos = Pos.union(pos,pos') 
            in (pos,ExtSyn.Forall(pos,SimpleType.Var'(),x,trm)) end
        fun exists ((pos,x),(pos',trm)) = 
            let val pos = Pos.union(pos,pos') 
            in (pos,ExtSyn.Exists(pos,SimpleType.Var'(),x,trm)) end
        val fixityitem_parser = maybe
        (fn (pos,tok) =>
            case tok of 
              FUSE => SOME(Opr(Infix(Right,6,fuse pos)))
            | RIGHTI => SOME(Opr(Infix(Right,4,righti pos)))
            | LEFTI => SOME(Opr(Infix(Right,4,lefti pos)))
            | BANG => SOME(Opr(Prefix(10, bang pos)))
            | GNAB => SOME(Opr(Prefix(10, gnab pos)))
            | ID (path,x) => SOME(Atm(pos,ExtSyn.Id(pos,path,x)))
            | LAMBDA _ => NONE
            | FORALL _ => NONE
            | EXISTS _ => NONE
            | PERIOD => NONE
            | LPAREN => NONE
            | RPAREN => NONE
            | tok => (print("Error: " ^ Pos.toString pos ^ "\n"); 
                      print("Unexpected token: " ^ token_to_string tok ^ "\n");
                      raise Match))
        val exp_parser = fix
        (fn exp_parser =>
            let
              val parens_parser = 
                  maybe (fn (pos,LPAREN) => SOME() 
                           | (pos,tok) => NONE)
                  >> exp_parser <<
                  maybe (fn (pos,RPAREN) => SOME() 
                          | (pos,tok) => 
                            (print("Error: " ^ Pos.toString pos ^ "\n");
                             print("Expected ')', found '" ^ 
                                   token_to_string tok ^ "'\n");
                             raise Match)) wth Atm
              val lambda_parser = 
                  maybe (fn (pos,LAMBDA x) => SOME (pos,x) | _ => NONE) 
                        && exp_parser wth (Atm o lambda)
              val forall_parser = 
                  maybe (fn (pos,FORALL x) => SOME (pos,x) | _ => NONE) 
                        && exp_parser wth (Atm o forall)
              val exists_parser = 
                  maybe (fn (pos,EXISTS x) => SOME (pos,x) | _ => NONE) 
                        && exp_parser wth (Atm o exists)
            in 
              parsefixityadj
                  (alt [fixityitem_parser,
                        parens_parser,
                        lambda_parser,
                        forall_parser,
                        exists_parser])
                  Left
                  (fn ((pos1,trm1),(pos2,trm2)) =>
                      let val pos = Pos.union(pos1,pos2) 
                      in (pos, ExtSyn.App(pos,trm1,trm2)) end)
            end)
        val force_period = 
            maybe (fn (pos,PERIOD) => SOME() 
                    | (pos,tok) => 
                      (print("Error: " ^ Pos.toString pos ^ "\n");
                       print("Expected '.', found '" ^ 
                             token_to_string tok ^ "'\n");
                       raise Match))
        val rule_parser = 
            maybe (fn (pos,ID([],x)) => SOME(pos,x) | _ => NONE)
            << maybe (fn (pos,COLON) => SOME() | _ => NONE)
            && exp_parser << force_period
                wth (fn ((pos,x),(pos',trm)) => 
                        ExtSyn.RULE(Pos.union(pos,pos'), x, trm))
        val exec_parser = 
            maybe (fn (pos,PERCENT("exec")) => SOME() | _ => NONE)
            >> exp_parser << force_period wth ExtSyn.EXEC
      in transform (alt [rule_parser,exec_parser]) end

  fun readfile f = 
      let
        val fs = StreamUtil.ftoUTF8stream f
        val pos0 = Pos.initposex f
        fun mark ("\n",pos) = 
            let val pos = Pos.nextline pos 
            in (("\n",pos), pos) end
          | mark (c,pos) =
            let val pos = Pos.nextchar pos
            in ((c,pos), pos) end
        val fs1 = StreamUtil.foldstream mark pos0 fs
        val fs2 = transform_tok1 fs1
        val fs3 = transform_tok2 fs2
        val fs4 = transform_tok3 fs3
        fun printtok (tok,pos) = 
            print (Pos.toString pos ^ " - " ^ token_to_string tok ^ "\n") 
        (* val () = Stream.app printtok fs4 *)
      in transform_decl fs4 end

  val x = fn () => readfile "examples/cbn.olf"      


end
