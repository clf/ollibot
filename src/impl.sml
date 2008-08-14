

structure CommentParen :> COMMENT_DEFINITION where type char = UChar.uchar = struct

open UChar
type char = uchar
val compare = Ord.compare
val c = fromChar
val s = fn s => map fromChar (explode s)
val line = 
    [{opening = s "%%", closing = c #"\n"}, 
     {opening = s "% ", closing = c #"\n"}]
val bracketed =
    [{opening = s "%{", closing = s "}%", ignore = true},
     {opening = s "%[", closing = s "]%", ignore = true},
     {opening = s "%(", closing = s ")%", ignore = false}]
val eof = [{opening = s "%."}, {opening = s "\^@"}]
val replace = NONE
val comments  = fn _ => ()
val unclosed  = fn p => (print ("UNCLOSED COMMENT "^Pos.toString p))
val unmatched = fn p => (print ("UNMATCHED COMMENT "^Pos.toString p))

end

structure CommentBracket :> COMMENT_DEFINITION where type char = UChar.uchar = struct

open UChar
type char = uchar
val compare = Ord.compare
val c = fromChar
val s = fn s => map fromChar (explode s)
val line = 
    [{opening = s "%%", closing = c #"\n"}, 
     {opening = s "% ", closing = c #"\n"}]
val bracketed =
    [{opening = s "%{", closing = s "}%", ignore = true},
     {opening = s "%[", closing = s "]%", ignore = false},
     {opening = s "%(", closing = s ")%", ignore = true}]
val eof = [{opening = s "%."}, {opening = s "\^@"}]
val replace = NONE
val comments  = fn _ => ()
val unclosed  = fn p => (print ("UNCLOSED COMMENT "^Pos.toString p))
val unmatched = fn p => (print ("UNMATCHED COMMENT "^Pos.toString p))

end

structure ParseParen = ParserFn(TokenizeFn(CommentParen))
structure ParseBracket = ParserFn(TokenizeFn(CommentBracket))
