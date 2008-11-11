structure Lambda :> LAMBDA = struct

  type cid = int
  val next = ref 0

  datatype knd = Type
  datatype typ = 
      TBase of cid
    | TArrow of typ * typ

  datatype dec = 
      ConDec    of {id: string, typ: typ}            (* c : At : type     *)
    | TypDec    of {id: string, knd: knd}            (* a : Kr : kind     *)
    | TypAbbrev of {id: string, typ: typ, knd: knd}  (* a : Kr = A : kind *)
            
  structure Map = 
  RedBlackMapFn(struct type ord_key = int val compare = Int.compare end)

  type signat = dec Map.map

  val sgnEmpty = Map.empty
  val sgnLookup : dec Map.map * int -> dec = valOf o Map.find
  val sgnAdd =
   fn (s, decl) => (next := !next + 1; (Map.insert(s,!next,decl),!next))


end
