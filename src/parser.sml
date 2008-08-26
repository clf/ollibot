signature PARSER = sig

type tokenstream

(* Create a token stream from a filename *)
val tokenize : string -> tokenstream

(* NONE if the stream is exhausted, SOME if the stream has a left edge *)
val pos : tokenstream -> Pos.pos option

val parseonce : tokenstream -> ((ParsedSyn.decl * Pos.pos) option) * tokenstream
val parseall : tokenstream -> (ParsedSyn.decl * Pos.pos) Stream.stream

end

functor ParserFn(T : TOKENIZER) :> PARSER = struct

structure Tok = T
structure P = ParsedSyn
open Tok
open ParsedSyn
open Parsing
open Global
infixr 4 << >>
infixr 3 &&
infix  2 -- ##
infix  2 wth suchthat return guard when
infixr 1 ||

fun is (a : tok) = satisfy(fn b => a = b)

val fst = fn (a,b) => a
val snd = fn (a,b) => b
val swap = fn (a,b) => (b,a)

val string = maybe (fn ID s => SOME s | _ => NONE)
val id = (!! string) wth (fn (s,pos) =>
                             if Char.isUpper (String.sub(s,0)) 
                             then UCid' (s,pos) else LCid' (s,pos))
val typ = (!! (maybe (fn TYPE e => SOME e | _ => NONE))) 
val var_decl =
 fn exp => alt [(!! string) wth Var,
                (!! string << is COLON && exp) wth VarTy]

(* FIXITY (from the TIGHTEST binding (x) to the WEAKEST binding (+))
 * ADJACENCY (application)
 * TYPE ASCRIPTION - 4
 * EQUALITY - 3
 * COMMA - 2
 * ARROW/BACKARROW - 1
 * LAMBDA/EXISTS/PI - 0 *)
val exp =
    fix (fn exp =>
            parsefixityadj
                (alt [id wth Atm,
                      (!!(is EXISTS >> var_decl exp << is DOT) && exp
                         wth Atm o Exists'),
                      (!!(is PI >> var_decl exp << is DOT) && exp
                         wth Atm o Exists'),
                      (!!(is LAMBDA >> var_decl exp << is DOT) && exp
                         wth Atm o Lam'),
                      (!!(is UNDERSCORE)) wth Atm o UnknownTerm' o snd,
                      (!!(is QUESTIONMARK)) wth Atm o UnknownType' o snd,
                      (is ARROW) return Opr(Infix(Right,1,Arrow')),
                      (is BACKARROW) return Opr(Infix(Right,1,Arrow' o swap)),
                      (is COMMA) return Opr(Infix(Right,2,Pair')),
                      (is UNIFY) return Opr(Infix(Non,3,Eq')),
                      (is COLON) return Opr(Infix(Non,4,HasType')),
                      (typ) wth Atm o Type',
                      (is LPAREN >> exp << is RPAREN) wth Atm])
                Left App')

val decl = 
    !!(alt [(string wth SOME) << is COLON && exp wth Decl,
            string << is EQ && exp wth Defn,
            succeed NONE && exp wth Decl]
           << is DOT)

type tokenstream = (tok * Pos.pos) Stream.stream

val pos = 
 fn ts => 
    case Stream.force ts of
      Stream.Nil => NONE
    | Stream.Cons((_,pos),_) => SOME pos

val tokenize = parsefile
val parseonce = parseonce decl 
val parseall = transform decl

end
