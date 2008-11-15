(* Marked wizards
 * Robert J. Simmons *)

(* BASIC RECURSIVE TYPES *)

signature TYP = sig
type t          (* A polymorphically-tagged type *)
type 't view       (* Views are described without regard to marking *)

val inj   : t view -> t 
val prj   : t -> t view
val map   : ('t1 -> 't2) -> 't1 view -> 't2 view
end

signature TYP_FULL = sig

structure T : TYP

(* Injections and projections for marked datatypes *)
val inj  : T.t T.view -> T.t 
val prj  : T.t -> T.t T.view 

(* Folding and unfolding for marked datatypes *)
val fold   : ('a T.view -> 'a) -> T.t -> 'a
val unfold : ('a -> 'a T.view) -> 'a -> T.t

end

signature MTYP = sig
type 'a t          (* A polymorphically-tagged type *)
type 't view       (* Views are described without regard to marking *)

val inj   : 'a t view * 'a -> 'a t 
val prj   : 'a t -> 'a t view * 'a
val map   : ('t1 -> 't2) -> 't1 view -> 't2 view
end

signature MTYP_FULL = sig

structure T : MTYP

(* Injections and projections for marked datatypes *)
val inj  : 'a T.t T.view * 'a -> 'a T.t 
val prj  : 'a T.t -> 'a T.t T.view * 'a

(* Folding and unfolding for marked datatypes *)
val fold   : ('a T.view * 'b -> 'a) -> 'b T.t -> 'a
val unfold : ('a -> 'a T.view * 'b) -> 'a -> 'b T.t

(* Re-mark a datatype in-place *)
val remark : ('old -> 'new) -> 'old T.t -> 'new T.t

(* Re-mark a datatype from the leaves outwards *)
val synth : ('new T.view * 'old -> 'new) -> 'old T.t -> 'new T.t

end



signature TYP2 = sig
type a
type b 
type ('a, 'b) aview
type ('a, 'b) bview
val inja : (a, b) aview -> a
val injb : (a, b) bview -> b
val prja : a -> (a, b) aview
val prjb : b -> (a, b) bview
val mapa : ('a1 -> 'a2) * ('b1 -> 'b2) -> ('a1, 'b1) aview -> ('a2, 'b2) aview
val mapb : ('a1 -> 'a2) * ('b1 -> 'b2) -> ('a1, 'b1) bview -> ('a2, 'b2) bview
end

signature TYP2_FULLA = sig
  type ('a, 'b) aview
  type ('a, 'b) bview
  type a 
  type b

  val fold   : (('a, 'b) aview -> 'a) * (('a, 'b) bview -> 'b) -> a -> 'a
  val unfold : ('a -> ('a, 'b) aview) * ('b -> ('a, 'b) bview) -> 'a -> a

  val inj  : (a, b) aview -> a

  val prj  : a -> (a, b) aview
  val prj1 : a -> (a, b) aview
  val prj2 : a -> ((a, b) aview, (a, b) bview) aview
  val prj3 : a -> (((a, b) aview, (a, b) bview) aview,
                     ((a, b) aview, (a, b) bview) bview) aview
end

signature TYP2_FULLB = sig
  type ('a, 'b) aview
  type ('a, 'b) bview
  type a 
  type b

  val fold   : (('a, 'b) aview -> 'a) * (('a, 'b) bview -> 'b) -> b -> 'b
  val unfold : ('a -> ('a, 'b) aview) * ('b -> ('a, 'b) bview) -> 'b -> b

  val inj : (a, b) bview -> b
  val prj : b -> (a, b) bview
  val prj2 : b -> ((a, b) aview, (a, b) bview) bview
  val prj3 : b -> (((a, b) aview, (a, b) bview) aview,
                     ((a, b) aview, (a, b) bview) bview) bview
end

signature TYP2_FULL = sig

structure T : TYP2 

structure FullA : 
          TYP2_FULLA 
              where type ('a, 'b) aview = ('a, 'b) T.aview
                and type ('a, 'b) bview = ('a, 'b) T.bview
                and type a = T.a and type b = T.b

structure FullB : TYP2_FULLB
              where type ('a, 'b) aview = ('a, 'b) T.aview
                and type ('a, 'b) bview = ('a, 'b) T.bview
                and type a = T.a and type b = T.b

end



