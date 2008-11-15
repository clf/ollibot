structure Global = struct

datatype polar = Pos | Neg
datatype perm = Lin | Pers
type kind = (polar * perm) option

exception Unimplemented of string

exception Error of string * Pos.pos

end
