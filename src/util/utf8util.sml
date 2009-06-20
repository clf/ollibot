structure UTF8Util :> UTF8_UTIL = struct

  open Byte
  open Word8
  open Word8Vector
  val (op +) = Int.+
  val (op >=) = Int.>=
  infix 5 >>
  exception UTF8

  fun multi n (vec,start,offset) =
      let val i = start + offset
      in
        if n = offset then ()
        else if i >= length vec then raise UTF8
        else if sub(vec,i) >> 0wx6 <> 0wx2 then raise UTF8
        else multi n (vec,start,offset+1) 
      end

  fun numbytes b = 
      if b >> 0wx7 = 0wx0 then 1
      else if b >> 0wx5 = 0wx6 then 2
      else if b >> 0wx4 = 0wxE then 3
      else if b >> 0wx3 = 0wx1E then 4
      else if b >> 0wx2 = 0wx3E then 5
      else if b >> 0wx1 = 0wx7E then 6
      else raise UTF8

  fun size vec start n =
      if start = length vec then n
      else 
        let val bytes = numbytes(sub(vec,start))
        in
          multi bytes (vec,start,1); 
          size vec (start + bytes) (n+1)
        end

  fun subvector(vec,start,bytes) = 
      unpackStringVec(Word8VectorSlice.slice(vec,start,SOME bytes))

  fun sub_impl (vec,m) start n =
      if start = length vec then raise Subscript
      else if m = n then 
        let val bytes = numbytes(sub(vec,start))
        in 
          multi bytes (vec,start,1); subvector(vec,start,bytes)
        end
      else 
        let val bytes = numbytes(sub(vec,start))
        in
          multi bytes (vec,start,1); 
          sub_impl (vec,m) (start + bytes) (n+1)
        end

  fun explode_impl vec start (list : String.string list) : String.string list = 
      if start = length vec then rev list
      else 
        let val bytes = numbytes(sub(vec,start))
        in
          explode_impl vec (start + bytes) (subvector(vec,start,bytes) :: list)
        end

  val size = fn s => size (Byte.stringToBytes s) 0 0
  val sub = fn (s,n) => sub_impl (Byte.stringToBytes s,n) 0 0
  val explode = fn s => explode_impl (Byte.stringToBytes s) 0 []
  val translate = fn f => fn s => String.concat (List.map f (explode s))

end
