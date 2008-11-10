signature LAMBDA = sig

  type cid
     
  datatype knd = Type
  datatype typ = 
      TBase of cid
    | TArrow of typ * typ

  datatype dec = 
      ConDec    of {id: string, typ: typ}            (* c : At : type     *)
    | TypDec    of {id: string, knd: knd}            (* a : Kr : kind     *)
    | TypAbbrev of {id: string, typ: typ, knd: knd}  (* a : Kr = A : kind *)
                   
  type signat
       
  val sgnEmpty : signat
  val sgnLookup : signat * cid -> dec
  val sgnAdd : signat * dec -> signat * cid

end

