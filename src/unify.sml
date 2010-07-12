structure SimpleType :> 
          sig
            type evar
            type styp
            datatype styp_view = 
                     Var of evar 
                   | Obj of string
                   | Arr of styp * styp
            exception Unify
            val unify : styp -> styp -> unit
            val bind : evar -> styp -> unit
            val prj : styp -> styp_view
            val NewVar : unit -> styp
            val Item : styp
            val Prop : styp
            val Arrow : styp * styp -> styp
          end =
struct

  open Global

  datatype evar = E of styp_view option ref
  and styp_view = 
      Var of evar 
    | Obj of string
    | Arr of styp_view * styp_view

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
          Obj x => Obj x
        | Arr(t1,t2) => Arr(t1,t2)
        | Var(E v) => lookup v
      end

  fun bind (E(v as ref NONE)) trm =
      (case prj trm of
         Var(E v') => if v = v' then () else v := SOME trm
       | _ => v := SOME trm)
    | bind _ _ = raise Match

  val NewVar = fn () => Var(E(ref NONE))
  val Item = Obj "i"
  val Prop = Obj "type"
  val Arrow = Arr

  fun occurs_check (e1 : evar) t2 = 
      case t2 of
        Var e2 => if e1 = e2 then raise Err("Cannon assign types; this is a useless error message and I apologize.\nPlease send the code to robsimmons@gmail.com and I will try to make future\nerror messages better.") else ()
      | Obj x => ()
      | Arr(t1,t2) => (occurs_check e1 t1; occurs_check e1 t2)

  exception Unify
  fun unify t1 t2 = 
      case (prj t1, prj t2) of
        (Var e1, t2) => bind e1 t2
      | (t1, Var e2) => bind e2 t1
      | (Obj x, Obj y) => if x <> y then raise Unify else ()
      | (Arr(t1,s1),Arr(t2,s2)) => (unify t1 t2; unify s1 s2)
      | _ => raise Unify
      
end
