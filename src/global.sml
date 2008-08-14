structure Global = struct

datatype polarity = Pos | Neg
datatype permeability = Linear | Persistant
type kind = (polarity * permeability) option

exception Error of string * Pos.pos

end
