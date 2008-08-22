(* Tokenizer 
 * Rob Simmons *)

signature TOKENIZER = sig

datatype tok = 
    LCURLY | RCURLY 
  | LPAREN | RPAREN
  | ARROW | BACKARROW
  | COMMA | DOT | COLON
  | UNDERSCORE 
  | EQ
  | LAMBDA | EXISTS | FORALL | PI
  | ID of string
  | DIRECTIVE of string
  | TYPE of Global.kind

val parsefile : string -> (tok * Pos.pos) Stream.stream

end

functor TokenizeFn(CD : COMMENT_DEFINITION where type char = UChar.uchar)
:> TOKENIZER = struct

structure Comment = CommentFn(CD)
open Global
open UChar
open Parsing
infixr 4 << >>
infixr 3 &&
infix  2 -- ##
infix  2 wth suchthat return guard when
infixr 1 ||

val openstream : string -> (uchar * Pos.pos) Stream.stream = 
  fn filename =>
    let 
      val ustr = stream(StreamUtil.ftobytestream filename)
      val pos = Pos.initposex filename

      fun advance (uchar,pos) = 
          case toChar uchar of 
            SOME #"\n" => Pos.nextline pos
          | _ => Pos.nextchar pos

      fun advance_pos (ustr,pos) = 
          case Stream.force ustr of 
            Stream.Nil => pos
          | Stream.Cons(uchar,_) =>
            advance(uchar,pos) 

      fun step pos ustr () =
          case Stream.force ustr of
            Stream.Nil => Stream.Nil
          | Stream.Cons(uchar,ustr) => 
            let val nextpos = advance_pos(ustr,pos) 
              val thispos = advance(uchar,pos) in
              Stream.Cons((uchar, Pos.union(pos,thispos)), 
                          Stream.delay (step thispos ustr))
            end
            
    in Stream.delay (step pos ustr) end

datatype tok = 
    LCURLY | RCURLY 
  | LPAREN | RPAREN
  | ARROW | BACKARROW
  | COMMA | DOT | COLON
  | UNDERSCORE
  | EQ
  | LAMBDA | EXISTS | FORALL | PI
  | ID of string
  | DIRECTIVE of string
  | TYPE of (polarity * permeability) option

exception Tokenize of string * Pos.pos
fun error s pos = raise Tokenize(s,pos)

fun eq c u = Ord.compare(fromChar c, u) = EQUAL
fun equ u u' = Ord.compare(u, u') = EQUAL
fun eqw w u = Ord.compare(fromWord w, u) = EQUAL

fun readSep c = 
    case prj c of 
      Char1 #"(" => SOME LPAREN 
    | Char1 #")" => SOME RPAREN
    | Char1 #"{" => SOME LCURLY
    | Char1 #"}" => SOME RCURLY
    | Char1 #"," => SOME COMMA
    | Char1 #":" => SOME COLON
    | Char1 #"=" => SOME EQ
    | CharM 0wx2203 => SOME EXISTS
    | CharM 0wx2200 => SOME FORALL
    | CharM 0wx3BB  => SOME LAMBDA
    | CharM 0wx3A0  => SOME PI
    | CharM 0wx2190 => SOME BACKARROW
    | CharM 0wx2192 => SOME ARROW
    | _ => NONE

fun isSep c = isSome (readSep c)

(* Symbols that can appear in identifiers *)
fun isSymb c =   
    case prj c of
      Char1 #"-" => false
    | Char1 #">" => false
    | Char1 #"<" => false
    | Char1 #"." => false
    | _ => not (isSep c) andalso not (isSpace c)

(* Symbols that can *start* identifiers *)
fun isSymbStart c = isSymb c andalso not (isNum c)

fun decodeLaTeX s = 
    case s of
      "lambda" => LAMBDA
    | "exists" => EXISTS
    | "forall" => FORALL
    | s => ID("\\"^s)

fun token(c,cs) = 
    case concat (map toString (c :: cs)) of 
      "_" => UNDERSCORE
    | "type" => TYPE NONE
    | "lin+" => TYPE (SOME(Pos,Linear))
    | "eph+" => TYPE (SOME(Pos,Linear))
    | "lin-" => TYPE (SOME(Neg,Linear))
    | "eph-" => TYPE (SOME(Neg,Linear))
    | "pers+" => TYPE (SOME(Pos,Persistant))
    | "pers-" => TYPE (SOME(Neg,Persistant))
    | s => ID s

val skipspace = repeat ((satisfy isSpace) return ())
val symbolstring = repeat1 (satisfy isSymb wth toString) wth concat
val satchar = fn c => satisfy (eq c)

val directive = satchar #"%" >> symbolstring wth DIRECTIVE
val escape = satchar #"\\" >> alt [symbolstring wth decodeLaTeX,
                                   satisfy (eq #"\\") return LAMBDA]
val separator = maybe readSep
val ident = satisfy isSymbStart && repeat (satisfy isSymb) wth token
val arrow = (repeat1 (satchar #"-") && satchar #">") return ARROW
val backarrow = (satchar #"<" && repeat1 (satchar #"-")) return BACKARROW
val dot = (satchar #"." && ignore_pos(satisfy isSpace)) return DOT

val tokenizer = 
    (ignore_pos skipspace >>
                alt [directive,escape,separator,ident,arrow,backarrow,dot])
    

val parsefile = fn f => transform (!!tokenizer) (Comment.filter (openstream f))

end

