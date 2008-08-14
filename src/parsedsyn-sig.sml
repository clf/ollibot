signature PARSED_SYN = sig

type 'a exp

datatype 'e var_decl = 
    Var of string * Pos.pos 
  | VarTy of (string * Pos.pos) * 'e

datatype 'e exp_view = 
    Exists of 'e var_decl * 'e
  | Pi of 'e var_decl * 'e
  | Lam of 'e var_decl * 'e
  | App of 'e * 'e
  | Arrow of 'e * 'e
  | Pair of 'e * 'e      
  | UCid of string 
  | LCid of string
  | Eq of 'e * 'e
  | Type of Global.kind

type pexp = Pos.pos exp

include MTYP_FULL 
                  where type 'a T.t = 'a exp
                    and type 'e T.view = 'e exp_view

val Exists' : (pexp var_decl * Pos.pos) * pexp -> pexp
val Pi' : (pexp var_decl * Pos.pos) * pexp -> pexp
val Lam' : (pexp var_decl * Pos.pos) * pexp -> pexp
val App' : pexp * pexp -> pexp
val Arrow' : pexp * pexp -> pexp
val Pair' : pexp * pexp -> pexp      
val UCid' : string * Pos.pos -> pexp
val LCid' : string * Pos.pos -> pexp
val Eq' : pexp * pexp -> pexp
val Type' : Global.kind * Pos.pos -> pexp

type decl = (string option * pexp option * pexp option) * Pos.pos

end
