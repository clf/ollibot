structure Lambda :> LAMBDA = struct

  type cid = int
  val next = ref 0
  structure Cid = struct type ord_key = cid val compare = Int.compare end

  datatype knd = Type

  datatype typ = 
      TBase of cid
    | TArrow of typ * typ

  datatype trm = 
      MLam of trm
    | MVar of int * trm list
    | MConst of cid * trm list 
    | MEvar of int * trm list

  datatype pos_atm = 
      PAtm of cid * trm list
    | PEq of trm * trm

  datatype rule = 
      RA_Arrow of {all: int, head: cid, pattern: trm list}
    | RA_Lolli of {all: int, head: cid, pattern: trm list}
    | RA_Shift of {exist: int, conc: pos_atm list}

  datatype dec = 
      ConDec    of {id: string, typ: typ}            (* c : At : type     *)
    | TypDec    of {id: string, knd: knd}            (* a : Kr : kind     *)
    | TypAbbrev of {id: string, typ: typ, knd: knd}  (* a : Kr = A : kind *)
    | Predicate of {id: string, knd: typ list * Global.perm}
    | RuleDec   of {rule: rule}
            
  structure Map = 
  RedBlackMapFn(struct type ord_key = int val compare = Int.compare end)

  type signat = dec Map.map

  val sgnEmpty = Map.empty
  val sgnLookup : dec Map.map * int -> dec = valOf o Map.find
  val sgnAdd =
   fn (s, decl) => (next := !next + 1; (Map.insert(s,!next,decl),!next))

end
