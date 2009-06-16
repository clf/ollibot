
structure GrowArray :> GROWARRAY =
struct

  type 'a growarray = (int * ('a option) Array.array) ref

  (* start with 16 cells, why not? *)
  fun empty () = ref (0, Array.array(16, NONE))

  fun clear r = r := (0, Array.array(16, NONE))

  fun growarray n i = ref (n, (Array.array(n, SOME i)))

  fun sub (ref (used, a)) n =
    if n < used andalso n >= 0
    then (case Array.sub(a, n) of
            NONE => raise Subscript
          | SOME z => z)
    else raise Subscript

  fun length (ref (l, _)) = l

  fun app f (r as ref (l, a)) =
      let
          fun ap n = 
              if n < l
              then ((case Array.sub(a, n) of
                        NONE => ()
                      | SOME x => f x);
                    ap (n + 1))
              else ()
      in
          ap 0
      end

  fun has (ref (used, a)) n = 
      if n < 0 then raise Subscript
      else n < used andalso Option.isSome(Array.sub(a, n))

  (* grow to accommodate at least n elements *)
  (* PERF this appears to be one element too conservative *)
  fun accommodate (r as ref(l, a)) n =
    if Array.length a >= (n + 1)
    then ()
    else
      let 
        fun nextpower x = 
          if x >= (n + 1) 
          then x
          else nextpower (x * 2)
        val ns = nextpower (Array.length a)
        val na = Array.tabulate(ns,
                                (fn i =>
                                 if i < l
                                 then Array.sub(a, i)
                                 else NONE))
      in
        r := (l, na)
      end

  fun update r n x =
    if n < 0 then raise Subscript
    else
      let 
        val _ = accommodate r n
        val (l, a) = !r
      in
        Array.update(a, n, SOME x);
        (* also update 'used' *)
        if n >= l
        then r := (n + 1, a)
        else ()
      end

  fun append (r as ref(n, _)) x =
    let
      val _ = accommodate r (n + 1)
      val (_, a) = !r
    in
      Array.update(a, n, SOME x);
      r := (n + 1, a)
    end

  fun truncate (r as ref(n, a)) x =
    if x > n
    then raise Subscript
    else r := (x, a)

  (* XXX should just discard the existing array, replacing
     with empty (and then change the semantics) to dispense with
     aliasing problems. *)
  fun finalize (ref (n, a)) =
    Array.tabulate (n, (fn x => case Array.sub(a, x) of
                                   NONE => raise Subscript
                                 | SOME z => z))

end