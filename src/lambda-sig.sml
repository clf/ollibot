signature LAMBDA = sig

  type cid = int
     
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
                   
  type signat
       
  val sgnEmpty : signat
  val sgnLookup : signat * cid -> dec
  val sgnAdd : signat * dec -> signat * cid

end

