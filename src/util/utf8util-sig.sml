signature UTF8_UTIL = sig

  (* An exception for encoding errors *)
  exception UTF8

  (* Number of UTF8 code points in a string *)
  val size : string -> int

  (* Extract the nth code point from a string *)
  val sub : string * int -> string
  

end
