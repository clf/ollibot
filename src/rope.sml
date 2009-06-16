structure AString = struct

  open Byte
  open Word8
  val (op +) = Int.+
  val (op >=) = Int.>=
  infix 5 >>
  open Word8Vector
  exception UTF8

  datatype part = 
      Natural of {s : vector, startp : int, size : int}
    | Inserted of {s : vector, startp : int, endp : int}
    | Tag of {s : vector, p : int}

  type string = part list
  
  val fromString = fn s => [Natural(stringToBytes s,0)]

  fun numbytes b =
      if b >> 0wx7 = 0wx0 then 1
      else if b >> 0wx5 = 0wx6 then 2
      else if b >> 0wx4 = 0wxE then 3
      else if b >> 0wx3 = 0wx1E then 4
      else if b >> 0wx2 = 0wx3E then 5
      else if b >> 0wx1 = 0wx7E then 6
      else raise UTF8

  (* Take a string, turn it into a Word8Vector, validate the UTF8 encoding *)
  fun validate (str,n_codepoint,n_byte) = 
      let 
        val vec = stringToBytes str
        fun multibyte (n_remaining,n_byte) =
            if n_remaining <= 0 then n_byte
            else if n_byte >= length vec then raise UTF8
            else if sub(vec,n_byte) >> 0wx6 <> 0wx2 then raise UTF8
            else multi n (vec,n_remaining-1,n_byte+1)
        fun loop (n_byte) =
            if n_byte = length vec then vec
            else loop (multibyte(numbytes(sub(n_byte)-1),n_byte+1))
      in loop 0 end

  val explode = 

end
