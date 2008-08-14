(* Robert J. Simmons
 * 
 * Implementation of functors for the "wizard" abstraction, defined generally.
 * Based on Celf by Anders Schack-Nielsen and Carsten Schürmann *)

(* Rather than building specalized recursors over recursive types, 
 * which we could do, we write functors that handle recursion over *)


(* == MakeTyp - Recursive wizard builder == *)

functor MakeTyp(T : TYP) :> 
TYP_FULL 
  where type T.t = T.t 
    and type 't T.view = 't T.view = struct
structure T = T
open T

(* fold iterates all the way down to the "leaves" of a data structure
 * by repeatedly projecting and then works back "up" by repeatedly applying
 * the step function. *)
fun fold step x = step (map (fold step) (prj x))

(* unfold builds a structure from the "root" to the leaves by generating
 * a new piece of data from the input and then repeatedly calling the
 * gen function on the output to build another step of the function. *)
fun unfold gen y = inj (map (unfold gen) (gen y))

(* multiple projections are rather easy, using map *)
fun prj1 x = prj x
fun prj2 x = map prj (prj x)
fun prj3 x = map prj2 (prj x)
end


functor MakeMTyp(T : MTYP) :> 
MTYP_FULL 
  where type 'a T.t = 'a T.t 
    and type 't T.view = 't T.view = struct
structure T = T

open T

fun fold (syns: 'a T.view * 'b -> 'a) (x: 'b t) = 
    let val (obj, mark) = prj x
    in syns (map (fold syns) obj,mark) end
      
fun unfold (gens: 'a -> 'a T.view * 'b) (y: 'a) = 
    let val (obj, mark) = gens y 
    in inj (map (unfold gens) obj, mark) end

fun snd obj = map (fn (a,b) => b) obj

fun synth (syns: 'new T.view * 'old -> 'new) x = 
    let 
      val (old_obj: 'old t view, old_mark: 'old) = prj x
      val (new_obj: 'new t view) = map (synth syns) old_obj
      val (new_mark: 'new) = syns (snd (map prj new_obj), old_mark)
    in inj (new_obj, new_mark) end

fun remark (f : 'new -> 'old) x =
    let 
      val (obj, mark) = prj x
    in inj (map (remark f) obj, f mark) end

end



functor MakeTyp2(T : TYP2) :> 
TYP2_FULL 
  where type T.a = T.a
    and type T.b = T.b
    and type ('a, 'b) T.aview = ('a, 'b) T.aview
    and type ('a, 'b) T.bview = ('a, 'b) T.bview = struct
structure T = T
open T

fun foldA (step as (stepA, stepB)) a = 
    stepA (T.mapa (foldA step, foldB step) (T.prja a)) 
and foldB (step as (stepA, stepB)) b = 
    stepB (T.mapb (foldA step, foldB step) (T.prjb b))

fun unfoldA (gen as (genA, genB)) a = 
    T.inja (T.mapa (unfoldA gen, unfoldB gen) (genA a))
and unfoldB (gen as (genA, genB)) b = 
    T.injb (T.mapb (unfoldA gen, unfoldB gen) (genB b))

fun prjA1 a = T.prja a
fun prjB1 b = T.prjb b
fun prjA2 a = T.mapa (prjA1,prjB1) (T.prja a)
fun prjB2 b = T.mapb (prjA1,prjB1) (T.prjb b)
fun prjA3 a = T.mapa (prjA2,prjB2) (T.prja a)
fun prjB3 b = T.mapb (prjA2,prjB2) (T.prjb b)

structure FullA = struct
  open T
  val fold = foldA
  val unfold = unfoldA

  val inj  = T.inja
  val prj  = prjA1
  val prj1 = prjA1
  val prj2 = prjA2
  val prj3 = prjA3
end

structure FullB = struct
  open T
  val fold = foldB
  val unfold = unfoldB

  val inj  = T.injb
  val prj  = prjB1
  val prj1 = prjB1
  val prj2 = prjB2
  val prj3 = prjB3
end


end
