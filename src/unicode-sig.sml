
(* Requires that the version specific "word" be able to hold at least the 
 * value 0wx1FFFFF in order to function correctly *)

(* Some useful unicode constants are given in unicode.txt *)

signature UTF8_CHAR = sig

type uchar
exception Encoding (* Rased for any out-of-range or illegal encodings *)

(* uchar_view deals with the single-char or single-word encoding of a uchar 
 * All words projected as CharM(w) WILL have w > 0wx7F, however, words injected
 * as CharM(w) need not have 0wx7F, though they must be less than 0wx1FFFFF
 * or Encoding will be raised. 
 * Therefore prj(inj(CharM(0wx41))) and prj(inj(Char1(#"A"))) will both 
 * evaluate to Char1(#"A"). *)

datatype uchar_view = 
    Char1 of char
  | CharM of word    

val prj : uchar -> uchar_view
val inj : uchar_view -> uchar


(* uchar_utf8 deals with the UTF-8 encoding of Unicode characters as 
 * defined by RFC ??? *)

(* One char:    0000 0000-0000 007F, 0xxxxxxx *)
(* Two chars:   0000 0080-0000 07FF, 110xxxxx 10xxxxxx *)
(* Three chars: 0000 0800-0000 FFFF, 1110xxxx 10xxxxxx 10xxxxxx *)
(* Four chars:  0001 0000-001F FFFF, 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx *)

datatype uchar_utf8 = 
    Word1 of Word8.word
  | Word2 of Word8.word * Word8.word
  | Word3 of Word8.word * Word8.word * Word8.word
  | Word4 of Word8.word * Word8.word * Word8.word * Word8.word

val prj_utf8 : uchar -> uchar_utf8
val inj_utf8 : uchar_utf8 -> uchar

val fromChar : char -> uchar
val fromInt  : int -> uchar
val fromWord : word -> uchar
val toChar : uchar -> char option
val toChar' : uchar -> char
val toString : uchar -> string
val stream : Word8.word Stream.stream -> uchar Stream.stream

structure Ord : ORD_KEY where type ord_key = uchar

(* Classic char utils *)
val chr : int -> uchar
val ord : uchar -> int
val minChar : uchar
val maxChar : uchar
val maxOrd : int
val isAscii : uchar -> bool
val isAlpha : uchar -> bool
val isNum : uchar -> bool
val isAlphaNum : uchar -> bool
val isSpace : uchar -> bool

end
