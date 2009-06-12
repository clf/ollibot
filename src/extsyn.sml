structure SimpleType :> 
          sig
            type evar
            type styp
            datatype styp_view = 
                     Var of evar | Item | Prop | Arrow of styp * styp
            exception Unify
            val unify : styp -> styp -> styp
            val bind : evar -> styp -> unit
            val prj : styp -> styp_view
            val Var' : unit -> styp
            val Item' : styp
            val Prop' : styp
            val Arrow' : styp * styp -> styp
          end =
struct

  datatype evar = E of styp_view option ref
  and styp_view = Var of evar | Item | Prop | Arrow of styp_view * styp_view

  type styp = styp_view
              
  fun prj typ = 
      let
        fun lookup (v as ref NONE) = Var(E v)
          | lookup (v as ref (SOME(Var(E v')))) = 
            let val typ = lookup v' 
            in v := SOME typ; typ end
          | lookup (v as ref (SOME typ)) = typ
      in
        case typ of
          Item => Item
        | Prop => Prop
        | Arrow(t1,t2) => Arrow(t1,t2)
        | Var(E v) => lookup v
      end

  fun bind (E(v as ref NONE)) trm =
      (case prj trm of
         Var(E v') => if v = v' then () else v := SOME trm
       | _ => v := SOME trm)
    | bind _ _ = raise Match

  val Var' = fn () => Var(E(ref NONE))
  val Item' = Item
  val Prop' = Prop
  val Arrow' = Arrow

  exception Unify
  fun unify t1 t2 = 
      case (prj t1, prj t2) of
        (Var e1, t2) => (bind e1 t2; t2)
      | (t1, Var e2) => (bind e2 t1; t2)
      | (Item, Item) => Item
      | (Prop, Prop) => Item
      | (Arrow(t1,s1),Arrow(t2,s2)) => Arrow(unify t1 t2, unify s1 s2)
      | _ => raise Unify
      
end

structure ExtSyn = struct

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
    | Bang of Pos.pos * term
    | Gnab of Pos.pos * term

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
      | Bang(p,_) => p
      | Gnab(p,_) => p

  datatype decl = 
      RULE of Pos.pos * string * term
    | EXEC of Pos.pos * term
              
end
