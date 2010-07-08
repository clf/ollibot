(* Ollibot — Robert J. Simmons and Frank Pfenning
 * Simple type inference and *)

structure ExtSyn = struct

  open Global

  datatype term = 
      App of Pos.pos * term * term    
    | Forall of Pos.pos * SimpleType.styp * string * term
    | Exists of Pos.pos * SimpleType.styp * string * term
    | Fuse of Pos.pos * term * term
    | Esuf of Pos.pos * term * term
    | Righti of Pos.pos * term * term
    | Lefti of Pos.pos * term * term 
    | Lambda of Pos.pos * SimpleType.styp * string * term
    | Id of Pos.pos * string list * string
    | One of Pos.pos 
    | Not of Pos.pos * term
    | Bang of Pos.pos * term
    | Gnab of Pos.pos * term
    | Neq of Pos.pos * term * term
    | Eq of Pos.pos * term * term
    | Arrow of Pos.pos * term * term
    | Type of Pos.pos * perm

  fun getpos term = 
      case term of
        App(p,_,_) => p
      | Forall(p,_,_,_) => p    
      | Exists(p,_,_,_) => p
      | Fuse(p,_,_) => p
      | Esuf(p,_,_) => p
      | Righti(p,_,_) => p
      | Lefti(p,_,_) => p
      | Lambda(p,_,_,_) => p
      | Id(p,_,_) => p
      | One p => p
      | Not(p,_) => p
      | Bang(p,_) => p
      | Gnab(p,_) => p
      | Neq(p,_,_) => p
      | Eq(p,_,_) => p
      | Arrow(p,_,_) => p
      | Type(p,_) => p

  datatype decl = 
      RULE of Pos.pos * string * term
    | EXEC of Pos.pos * int option * term
    | TRACE of Pos.pos * int option * term
              
end
