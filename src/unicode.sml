(* Requires that the version specific "word" be able to hold at least the 
 * value 0wx1FFFFF in order to function correctly *)

structure UTF8Char :> UTF8_CHAR = struct

(* Structure of a Word(32,31,...,27) representing a UTF8Char
 * ??????xx 000dddcc ccccbbbb bbaaaaaa 
 * aaaaaa - 1st least significant digits in a multi-word encoding 
 * bbbbbb - 2nd least significant digits in a multi-word encoding 
 * cccccc - 3rd least significant digits in a multi-word encoding
 * ddd    - Most significant digits in the 4-word encoding
 * xx     - Number of extra digits needed (1-3) *)

open Word
val orb8 = Word8.orb
val andb8 = Word8.andb
type byte = Word8.word

(* Bit-shifting binds tighter than xor/or/and *)
infix 4 << >> ~>> < >
infix 3 xorb orb andb orb8 andb8

(* One char:    0000 0000-0000 007F, 0xxxxxxx *)
(* Two chars:   0000 0080-0000 07FF, 110xxxxx 10xxxxxx *)
(* Three chars: 0000 0800-0000 FFFF, 1110xxxx 10xxxxxx 10xxxxxx *)
(* Four chars:  0001 0000-001F FFFF, 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx *)
val char1_mask = notb 0wx7F
val char2_mask = notb 0wx7FF
val char3_mask = notb 0wxFFFF
val char4_mask = notb 0wx1FFFFF

type uchar = word

exception Encoding

datatype uchar_view = 
    Char1 of char
  | CharM of word    

val prj =
 fn w =>
    case w >> 0wx18 of
      0wx0 => Char1(chr(toInt w))
    | _ => CharM(w andb 0wx1FFFFF)

val inj = 
 fn Char1(c) => fromInt(ord(c))
  | CharM(w) =>
    if      (w andb char1_mask) = 0wx0 then w
    else if (w andb char2_mask) = 0wx0 then 0wx1000000 orb w
    else if (w andb char3_mask) = 0wx0 then 0wx2000000 orb w
    else if (w andb char4_mask) = 0wx0 then 0wx3000000 orb w
    else raise Encoding


(* uchar_utf8 deals with the UTF-8 encoding of Unicode characters as 
 * defined by RFC ??? *)


datatype uchar_utf8 = 
    Word1 of byte
  | Word2 of byte * byte
  | Word3 of byte * byte * byte
  | Word4 of byte * byte * byte * byte

val toByte : word -> byte =
 fn w => Word8.fromLargeWord(toLargeWord(w andb 0wxFF))
val fromByte : byte -> word = 
 fn b => fromLargeWord(Word8.toLargeWord b)

val prj_utf8 =
 fn w =>
    case w >> 0wx18 of 
      0wx0 => Word1(toByte w)
    | 0wx1 => Word2(0wxC0 orb8 (toByte(w >> 0wx6 andb 0wx1F)),
                    0wx80 orb8 (toByte(w andb 0wx3F)))
    | 0wx2 => Word3(0wxE0 orb8 (toByte(w >> 0wxC andb 0wxF)),
                    0wx80 orb8 (toByte(w >> 0wx6 andb 0wx3F)),
                    0wx80 orb8 (toByte(w andb 0wx3F)))
    | 0wx3 => Word4(0wxF0 orb8 (toByte(w >> 0wx12 andb 0wx7)),
                    0wx80 orb8 (toByte(w >> 0wxC andb 0wx3F)),
                    0wx80 orb8 (toByte(w >> 0wx6 andb 0wx3F)),
                    0wx80 orb8 (toByte(w andb 0wx3F)))
    | _ => raise Encoding

val isMult = fn w => Word8.>>(w,0wx6) = 0wx2

val fromWord1 : byte -> word =
 fn w1 => 
    if Word8.>>(w1,0wx7) = 0wx0 then fromByte w1 else raise Encoding

val fromWord2 : byte * byte -> word =
 fn (w1,w2) => 
    if Word8.>>(w1,0wx5) = 0wx6 andalso isMult w2
    then (fromByte(w1 andb8 0wx1F) << 0wx6) orb 
         (fromByte(w2 andb8 0wx3F)) orb 0wx1000000
    else raise Encoding

val fromWord3 : byte * byte * byte -> word =
 fn (w1,w2,w3) =>
    if Word8.>>(w1,0wx4) = 0wxE andalso isMult w2 andalso isMult w3
    then (fromByte(w1 andb8 0wx0F) << 0wxC) orb
         (fromByte(w2 andb8 0wx3F) << 0wx6) orb
         (fromByte(w3 andb8 0wx3F)) orb 0wx2000000
    else raise Encoding

val fromWord4 : byte * byte * byte * byte -> word =
 fn (w1,w2,w3,w4) =>
    if Word8.>>(w1,0wx3) = 0wx1E 
       andalso isMult w2 andalso isMult w3 andalso isMult w4
    then (fromByte(w1 andb8 0wx07) << 0wx14) orb
         (fromByte(w2 andb8 0wx3F) << 0wxC) orb
         (fromByte(w3 andb8 0wx3F) << 0wx6) orb
         (fromByte(w3 andb8 0wx3F)) orb 0wx3000000 
    else raise Encoding

val inj_utf8 = 
 fn Word1 w => fromWord1 w
  | Word2 w => fromWord2 w
  | Word3 w => fromWord3 w
  | Word4 w => fromWord4 w

val btc = Byte.byteToChar

val fromInt = fn i => inj(CharM(fromInt i))
val fromWord = fn w => inj(CharM w)
val fromChar = fn c => inj(CharM(fromInt(ord c)))

val toString = 
 fn w =>
    String.implode
    (case prj_utf8 w of
       Word1(w) => [btc w]
     | Word2(w1,w2) => [btc w1, btc w2]
     | Word3(w1,w2,w3) => [btc w1, btc w2, btc w3]
     | Word4(w1,w2,w3,w4) => [btc w1, btc w2, btc w3, btc w4])

val toChar = fn w => case prj w of Char1(c) => SOME c | _ => NONE
val toChar' = fn w => case prj w of Char1(c) => c | _ => raise Encoding

val chr = fromInt
val ord = toInt
val minChar = 0wx0
val maxChar = 0wx31FFFFF
val maxOrd = 2097151
val isAscii = fn w => w >> 0wx18 = 0wx0
val isAlpha = 
 fn w => (w >= 0wx61 andalso w <= 0wx7a) orelse
         (w >= 0wx41 andalso w <= 0wx5A) 
val isNum = 
 fn w => (w >= 0wx30 andalso w <= 0wx39)
val isAlphaNum = 
 fn w => isAlpha w orelse isNum w
         
val isLargeSpace =
 fn 0wx2001680 => true (* OGHAM SPACE MARK *)
  | 0wx200180E => true (* MONGOLIAN VOWEL SEPARATOR *)
  | 0wx200202F => true (* NARROW NO-BREAK SPACE *)
  | 0wx200205F => true (* MEDIUM MATHEMATICAL SPACE *)
  | 0wx2003000 => true (* IDEOGRAPHC SPACE *)
  | w => w >= 0wx2002000 andalso w <= 0wx200200A
val isSpace =
 fn 0wx9 => true
  | 0wxA => true
  | 0wxD => true
  | 0wx20 => true
  | 0wxA0 => true
  | w => w >= 0wx2001680 andalso isLargeSpace w

fun stream bs =
    Stream.delay(fn () =>
             case Stream.force bs of
               Stream.Nil => Stream.Nil
             | Stream.Cons(w1,bs) =>
               if Word8.>>(w1,0wx7) = 0wx0 then Stream.Cons(fromWord1 w1,
                                                            stream bs)
               else if Word8.>>(w1,0wx5) = 0wx6 
               then (case Stream.force bs of 
                       Stream.Nil => raise Encoding
                     | Stream.Cons(w2,bs) => 
                       Stream.Cons(fromWord2(w1,w2), 
                                   stream bs))
               else if Word8.>>(w1,0wx4) = 0wxE
               then (case Stream.force bs of
                       Stream.Nil => raise Encoding
                     | Stream.Cons(w2,bs) => 
                       (case Stream.force bs of 
                          Stream.Nil => raise Encoding
                        | Stream.Cons(w3,bs) => 
                          Stream.Cons(fromWord3(w1,w2,w3), 
                                      stream bs)))
               else (case Stream.force bs of 
                       Stream.Nil => raise Encoding
                     | Stream.Cons(w2,bs) => 
                       (case Stream.force bs of
                          Stream.Nil => raise Encoding
                        | Stream.Cons(w3,bs) =>
                          (case Stream.force bs of 
                             Stream.Nil => raise Encoding
                           | Stream.Cons(w4,bs) => 
                             Stream.Cons(fromWord4(w1,w2,w3,w4),
                                         stream bs)))))

structure Ord = struct
type ord_key = uchar
val compare = Word.compare
end

end

structure UChar = UTF8Char
