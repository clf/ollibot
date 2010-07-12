signature PARSE = sig
  
  type token
  val token_to_string : token -> string
  val file_to_tokenstream : string -> (token * Pos.pos) Stream.stream 
  val string_to_tokenstream : string -> (token * Pos.pos) Stream.stream
  val decl_parser : (ExtSyn.decl, token) Parsing.parser

end

structure Parse :> PARSE = struct

  open Global
  open Parsing
  
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ## --! ##!
  infix  2 wth suchthat return guard when
  infixr 1 ||

  fun p ##! s = p ## (fn p => raise ErrPos(p,s))

  datatype token = 
      PERIOD | LPAREN | RPAREN 
    | BANG | GNAB | NEG | ONE
    | COMMA | EQ | NEQ
    | RIGHTI | LEFTI | FUSE
    | LOLLI | TENSOR
    | ARROW | CONJ 
    | PERCENT of string | COLON | WS
    | LAMBDA | FORALL | PI | EXISTS
    | ID of string list * string 

  fun token_to_string token = 
      case token of 
        PERIOD => "." | LPAREN => "(" | RPAREN => ")" 
      | BANG => "!" | GNAB => "¡" | NEG => "¬" | ONE => "<1>"
      | COMMA => "," | EQ => "==" | NEQ => "<>"
      | RIGHTI => "->>" | LEFTI => ">->" | FUSE => "·"
      | LOLLI => "->>" | TENSOR => "⊗" 
      | ARROW => "->" | CONJ => "∧" 
      | PERCENT x => "%" ^ x | COLON => ":" | WS => ""
      | LAMBDA => "λ" | FORALL => "∀" | PI => "Π" | EXISTS => "∃"
      | ID(path,x) => concat (map (fn x => x ^  ".") path) ^ x 

  fun err_expected_tok expected = 
      any --! 
          (fn (t,p) =>
              raise ErrPos(p,"Expected '" ^ token_to_string expected ^
                             "' found '" ^ token_to_string t ^ "'."))
      || done() --! 
          (fn ((),p) =>
              raise ErrPos(p,"Expected '" ^ token_to_string expected ^
                             "' found end of file."))

  (* Literate transform: remove all non-literate parts from a file *)
  val transform_tok0 = 
   fn fs =>
      let
        fun newline fs () = 
            case Stream.force fs of 
              Stream.Nil => Stream.Nil
            | Stream.Cons((">",_),fs) => codeline fs ()
            | _ => textline fs ()

        and textline fs () = 
            case Stream.force fs of
              Stream.Nil => Stream.Nil
            | Stream.Cons(("\n",_),fs) => newline fs ()
            | Stream.Cons(_,fs) => textline fs ()

        and codeline fs () =
            case Stream.force fs of
              Stream.Nil => Stream.Nil
            | Stream.Cons(("\n",p),fs) => 
              Stream.Cons(("\n",p),Stream.delay(newline fs))
            | Stream.Cons((c,p),fs) =>
              Stream.Cons((c,p),Stream.delay(codeline fs))
      in Stream.delay(newline fs) end 
            

  (* Transform 1: 
   *** Turn comments and whitespace into whitespace; consolidate whitespace
   *)
  val transform_tok1 = 
    let
      val nliteral = fn c : string => satisfy (fn x => not (x = c))
      val rliteral = fn c : string => repeat (literal c) >> succeed c
      val rliteral1 = fn c : string => repeat1 (literal c) >> succeed c
      val ws = 
       fn " " => true | "\t" => true | "\n" => true | _ => false

      (* Does a lot of work to check that it's not ----} *)
      val linecomment : (string, string) parser = 
       literal "-" >> rliteral1 "-" >>
       (alt [done "\n",
             literal "\n",
             nliteral "}" >> repeat (nliteral "\n") >> 
                      (done "\n" || literal "\n")])

      (* {= Equals comments =} are for headings and are limited *)
      val comment_eq : (string, string) parser = (* Not recursive *)
       (literal "{" >> literal "=") --! (fn (_,p) => 
       repeat (alt
         [literal "}" 
           --! (fn (_,p2) => 
            raise ErrPos(p2, "Character '}' not permitted in {= Comment =}")),
          literal "-" >> literal "-" 
           --! (fn (_,p2) =>
            raise ErrPos(p2, "Line comment can't start in {= Comment =}")),
          rliteral "=" >> (literal "{" 
           --! (fn (_,p2) => 
            raise ErrPos(p2, "Character '{' not permitted in {= Comment =}"))),
          rliteral "=" >> nliteral "}",
          done "\n"
           --! (fn _ => raise ErrPos(p, "Unclosed {= Comment =} opened here"))])
       >> rliteral1 "=" >> literal "}")

      (* {- Minus comments -} are recursive and are supposed to be general *)
      val comment_minus_special_case = 
       (literal "{" >> literal "-" >> rliteral1 "-" >> literal "}")
      val noncomment = 
       fn "{" => false | "-" => false | "=" => false | _ => true
      val comment_minus : (string, string) parser = 
       fix (fn comment_minus => (* Recursive *)
       (literal "{" >> rliteral1 "-") --! (fn (_,p) => 
       repeat (alt
         [satisfy noncomment,
          (* Case EOF *) 
          done "\n"
           --! (fn _ => raise ErrPos(p, "Unclosed {- Comment -} opened here")),
          (* Case OPEN_BRACKET *)
          comment_eq, 
          comment_minus_special_case,
          comment_minus,
          literal "{",
          (* Case EQUALS *)
          rliteral1 "=" >> done "\n"
           --! (fn _ => raise ErrPos(p, "Unclosed {- Comment -} opened here")),
          rliteral1 "=" >> literal "}"
           --! (fn (_,p) => 
             raise ErrPos(p,"Comment opened with \"{-\" closed with \"=}\"")),
          rliteral1 "=", 
          (* Case MINUS - may not handle *)
          linecomment,
          rliteral1 "-" >> done "\n"
           --! (fn _ => raise ErrPos(p, "Unclosed {- Comment -} opened here")),
          rliteral1 "-" >> rliteral1 "=" >> literal "}"
           --! (fn (_,p) => 
             raise ErrPos(p,"Comment opened with \"{-\" closed with \"=}\"")),
          rliteral1 "-" >> nliteral "}"])
       >> rliteral1 "-" >> literal "}"
           ##! "Some funky error" ))
      val parser = alt
         [repeat1 (satisfy ws) >> succeed " ",
          linecomment >> succeed " ",
          comment_eq >> succeed " ",
          comment_minus_special_case >> succeed " ",
          comment_minus >> succeed " ",
          literal "{" --! 
           (fn (_,p) => 
               raise ErrPos(p, "Character '{' only appears in comments")),
          literal "}" --! 
           (fn (_,p) => 
               raise ErrPos(p, "Character '}' only appears in comments")),
          any]
    in transform (!! parser) end
       
  (* Transform 2: 
   *** Consolidate identifiers and separate separators
   *)
  val transform_tok2 = 
      let
        val sep = 
            fn "." => true | "(" => true | ")" => true 
             | "!" => true | "¡" => true | "," => true 
             | "↠" => true | "↣" => true 
             | "·" => true | "•" => true | "*" => true
             | "⊸" => true | "⊗" => true
             | "→" => true | "∧" => true | "¬" => true
             | "λ" => true | "∀" => true | "∃" => true 
             | "\\" => true | "%" => true | ":" => true
             | " " => true
             | _ => false
        val parser = 
         alt [satisfy sep,
              repeat1 (satisfy (not o sep)) wth concat]
      in transform (!! parser)
      end

  (* Transform 3:
   *** Tokenize separators
   *** Read TeX-style characters (XXX not implemented XXX)
   *** Ensure binders are followed by identifiers and periods (XXX this choice
   ***   could be problematic if we need to attach types to binders...)
   *** Resolve identifier paths (XXX not implemented XXX)
   *)
  val transform_tok3 = 
      let
        fun not str pos = 
            raise 
              ErrPos(pos, "Binder " ^ " not followed by identifier + period")
        fun backslash pos = raise ErrPos(pos, "Backslash not permitted")
        (* *)
        fun idgrab [] = raise Err "Bad argument to internal parsing function"
          | idgrab [x] = ([], x)
          | idgrab (x :: xs) = let val (ys, id) = idgrab xs in (x :: ys, id) end
        val idparser = 
            any && repeat (literal "." >> maybe (fn " " => NONE | x => SOME x))
            wth (ID o idgrab o op ::)
        val tokenparser = 
            alt [literal "." >> succeed PERIOD,
                 literal "(" >> succeed LPAREN,
                 literal ")" >> succeed RPAREN,
                 literal "!" >> succeed BANG,
                 literal "¡" >> succeed GNAB,
                 literal "¬" >> succeed NEG,
                 literal "<1>" >> succeed ONE,
                 literal "," >> succeed COMMA,
                 literal "==" >> succeed EQ,
                 literal "<>" >> succeed NEQ,
                 literal "↠" >> succeed RIGHTI,
                 literal "->>" >> succeed RIGHTI,
                 literal "↣" >> succeed LEFTI,
                 literal ">->" >> succeed LEFTI,
                 literal "*" >> succeed FUSE,
                 literal "·" >> succeed FUSE,
                 literal "•" >> succeed FUSE,
                 literal "-o" >> succeed LOLLI,
                 literal "⊸" >> succeed LOLLI,
                 literal "⊗" >> succeed TENSOR,
                 literal "->" >> succeed ARROW,
                 literal "→" >> succeed ARROW,
                 literal "∧" >> succeed CONJ, 
                 literal "%" >> any wth PERCENT,
                 literal ":" >> succeed COLON,
                 literal " " >> succeed WS,
                 literal "->>" >> succeed RIGHTI,
                 literal ">->" >> succeed LEFTI,
                 literal "\\" >> succeed LAMBDA,
                 literal "λ" >> succeed LAMBDA,
                 literal "All" >> succeed FORALL,
                 literal "∀" >> succeed FORALL,
                 literal "Pi" >> succeed PI,
                 literal "Π" >> succeed PI,
                 literal "Exists" >> succeed EXISTS,
                 literal "∃" >> succeed EXISTS,
                 (* (!! (literal "\\") -- (fn (_,pos) => backslash pos)), *)
                 idparser] 
      in transform (!! tokenparser) end 

  (* Transform 4: Filter whitespace *)
  val transform_tok4 = 
      let 
        fun filter s () = 
            case Stream.force s of
              Stream.Nil => Stream.Nil
            | Stream.Cons((WS,_),s) => filter s ()
            | Stream.Cons((tok,pos : Pos.pos),s) => 
              Stream.Cons ((tok,pos),Stream.delay (filter s))
      in fn s => Stream.delay (filter s) end
 
  (* Main parser *)
  val decl_parser =
      let 
        fun unop constructor pos (trm1, pos1) = 
            let val pos = Pos.union(pos,pos1)
            in (constructor (pos,trm1),pos) end
        val bang = unop ExtSyn.Bang
        val gnab = unop ExtSyn.Gnab
        val neg  = unop ExtSyn.Not

        fun binop constructor pos ((trm1,pos1),(trm2,pos2)) = 
            let val pos = Pos.union(Pos.union(pos,pos1),pos2)
            in (constructor (pos,trm1,trm2),pos) end
        val righti = binop ExtSyn.Righti
        val lefti  = binop ExtSyn.Lefti
        val fuse   = binop ExtSyn.Fuse
        val eq     = binop ExtSyn.Eq
        val neq    = binop ExtSyn.Neq
        val arrow  = binop ExtSyn.Arrow

        fun binder constructor ((x,pos),(trm,pos')) = 
            let val pos = Pos.union(pos,pos') 
            in (constructor (pos,SimpleType.NewVar(),x,trm),pos) end
        val lambda = binder ExtSyn.Lambda 
        val pi     = binder ExtSyn.Forall (* XXX Same as ∀ *)
        val forall = binder ExtSyn.Forall
        val exists = binder ExtSyn.Exists

        val fixityitem_parser = 
        get (fn pos => 
          maybe (fn tok =>
            case tok of 
              FUSE => SOME(Opr(Infix(Right,6,fuse pos)))
            | RIGHTI => SOME(Opr(Infix(Right,4,righti pos)))
            | LEFTI => SOME(Opr(Infix(Right,4,lefti pos)))
            | BANG => SOME(Opr(Prefix(10, bang pos)))
            | GNAB => SOME(Opr(Prefix(10, gnab pos)))
            | NEG => SOME(Opr(Prefix(10, neg pos)))
            | ONE => SOME(Atm(ExtSyn.One pos, pos))
            | ID (path,x) => SOME(Atm(ExtSyn.Id(pos,path,x),pos))
            | ARROW => SOME(Opr(Infix(Right,4,arrow pos)))
            | EQ => SOME(Opr(Infix(Right,12,eq pos)))
            | NEQ => SOME(Opr(Infix(Right,12,neq pos)))
            | LAMBDA => NONE
            | FORALL => NONE
            | PI => NONE
            | EXISTS => NONE
            | PERIOD => NONE
            | LPAREN => NONE
            | RPAREN => NONE
            | tok => 
              raise ErrPos(pos,"Unexpected token: " ^ token_to_string tok)))
       
        val exp_parser = fix
        (fn exp_parser =>
            let
              val parens_parser = 
                  get (fn pos => 
                    maybe (fn LPAREN => SOME() | tok => NONE))
                  >> !! exp_parser <<
                  get (fn pos => 
                    maybe (fn RPAREN => SOME() 
                            | tok => raise ErrPos(pos,"Expected ')', found '"
                                                      ^ token_to_string tok)))
                  wth Atm
              val arg_parser = 
                  !! (maybe (fn ID([],x) => SOME (x,NONE)
                              | _ => NONE))
                     << literal PERIOD
                     wth ExtSyn.Decl o (fn ((y,z),x) => (x,y,z))
              val binder_parser = !! arg_parser && !! exp_parser
              val lambda_parser = literal LAMBDA >> binder_parser wth lambda
              val forall_parser = literal FORALL >> binder_parser wth forall
              val pi_parser     = literal PI >> binder_parser wth pi
              val exists_parser = literal EXISTS >> binder_parser wth exists
            in 
              parsefixityadj
                  (alt [fixityitem_parser,
                        parens_parser,
                        lambda_parser wth Atm,
                        forall_parser wth Atm,
                        exists_parser wth Atm])
                  Left
                  (fn ((trm1,pos1),(trm2,pos2)) =>
                      let val pos = Pos.union(pos1,pos2) 
                      in (ExtSyn.App(pos,trm1,trm2),pos) end)
            end wth (fn (trm,pos) => trm))
        val force_period = 
            get (fn pos => 
              maybe (fn PERIOD => SOME() 
                      | tok => 
                        raise ErrPos(pos,"Expected '.', found '"
                                       ^ token_to_string tok)))
        val rule_parser = 
            get (fn pos => 
              maybe (fn ID([],x) => SOME(x,pos) | _ => NONE))
            << maybe (fn COLON => SOME() | _ => NONE)
            && !! exp_parser << force_period 
                wth (fn ((x,pos),(trm,pos')) => 
                        ExtSyn.RULE(Pos.union(pos,pos'), x, trm))
        val numparser =
         fn FUSE => SOME(NONE) (* XXX bit of a hack here to read "*" *)
          | ID([],id) => 
            (case Int.fromString id of NONE => NONE | SOME i => SOME(SOME i))
          | _ => NONE
        val exec_parser = 
            get (fn pos => 
              maybe (fn PERCENT("exec") => SOME() | _ => NONE)
              >> maybe numparser && exp_parser << force_period 
                wth (fn (n,x) => ExtSyn.EXEC(pos,n,x)))
        val trace_parser = 
            get (fn pos => 
              maybe (fn PERCENT("trace") => SOME() | _ => NONE)
              >> maybe numparser && exp_parser << force_period 
                wth (fn (n,x) => ExtSyn.TRACE(pos,n,x)))
      in alt [rule_parser,exec_parser,trace_parser] end

  fun markstream f fs = 
      let
        val pos0 = Pos.initposex f
        fun mark ("\n",pos) = 
            let val pos' = Pos.nextline pos 
            in
              liveprint "\n";
              (("\n", Pos.union(pos,pos')), pos')
            end
          | mark (c, pos) =
            let val pos' = Pos.nextchar pos
            in
              liveprint c;
              ((c, Pos.union(pos,pos')), pos')
            end
      in StreamUtil.foldstream mark pos0 fs end

  fun stream_to_tokenstream f fs = 
      let 
        val fs1 = markstream f fs
        val fs2 = transform_tok1 fs1
        val fs3 = transform_tok2 fs2
        val fs4 = transform_tok3 fs3 
        val fs5 = transform_tok4 fs4
      in fs5 end

  fun literate_to_tokenstream f fs = 
      let
        val fs0 = markstream f fs
        val fs1 = transform_tok0 fs0
        val fs2 = transform_tok1 fs1
        val fs3 = transform_tok2 fs2
        val fs4 = transform_tok3 fs3 
        val fs5 = transform_tok4 fs4
      in fs5 end

  (* Default is to non-literate code *)
  fun file_to_tokenstream f = 
      case OS.Path.splitBaseExt f of
        {ext=SOME "lolf",...} => 
        literate_to_tokenstream f (StreamUtil.ftoUTF8stream f)
      | _ => stream_to_tokenstream f (StreamUtil.ftoUTF8stream f)

  (* Assumed non-literate code *)
  fun string_to_tokenstream s =
      stream_to_tokenstream "" (StreamUtil.ltostream(UTF8Util.explode s))

(*
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
*)
  fun readfile f = raise Match


end
