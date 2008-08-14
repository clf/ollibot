(* Comment filters
 * Rob Simmons
 * 
 * Instead of placing comment removal as a part of the tokenizing pass, 
 * implements a separate first-pass comment filter directly on streams. *)

signature COMMENT_DEFINITION = sig

type char 
val compare : char * char -> order

(* Line comments ignore *all* characters until the newline character is 
 * encountered. *)
val line : {opening: char list, closing: char} list

(* Bracketed comments create a hierarchical structure around the text; however,
 * bracketed comments where ignore=false are just treated as part of the file,
 * and only the delimiters are stripped from the stream. *)
val bracketed : {opening: char list, closing: char list, ignore: bool} list

(* End of file comments are character strings that preclude any further
 * consideration of the file. *)
val eof : {opening: char list} list

(* NONE if comments are *strictly* removed, SOME(char) if comments
 * are to be replaced with char *)
val replace : char option

(* Side Effecting Operations *)
val comments : Pos.pos -> unit (* Run at least once on every comment's pos *)
val unclosed : Pos.pos -> unit (* Run for an unclosed bracketed comment *)
val unmatched : Pos.pos -> unit (* Run for a non-matching close comment *)

end

signature COMMENT = sig

type char
structure Def : COMMENT_DEFINITION where type char = char

val filter : (char * Pos.pos) Stream.stream -> (char * Pos.pos) Stream.stream

end
