
signature UTIL =
sig

  val both : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
  val doboth : ('a -> 'b) -> ('a -> 'c) -> 'a -> unit

  (* run f on every integer lo..hi inclusive *)
  val for  : int -> int -> (int -> 'a) -> unit
  val ford : int -> int -> 'a -> (int * 'a -> 'a) -> 'a

  datatype ('a, 'b) sum = A of 'a | B of 'b

  val sift : ('a -> ('curds, 'whey) sum) -> 'a list -> 'curds list * 'whey list
  val mapa : ('a -> 'c) -> ('a, 'b) sum -> ('c, 'b) sum
  val mapb : ('b -> 'c) -> ('a, 'b) sum -> ('a, 'c) sum

  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

  val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
  val curry3   : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

  val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
  val curry4   : ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e

  type 'a orderer = ('a * 'a) -> order

  (* pow n m   gives n^m, defining 0^0 to be 1 *)
  val pow : int -> int -> int

  val order_compare : order orderer
  val bool_compare : bool orderer
  val sum_compare : 'a orderer -> 'b orderer -> ('a, 'b) sum orderer

  val option_compare : 'a orderer -> 'a option orderer

  val lex_order : ('a * 'b -> order) -> ('c * 'd -> order) ->
                  (('a * 'c) * ('b * 'd) -> order)

  val lex_list_order : ('a * 'b -> order) -> ('a list * 'b list -> order)

  (* swap the order of curried arguments *)

  val c21 : ('a1 -> 'a2 -> 'b) ->
            ('a2 -> 'a1 -> 'b)

  val c132 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a1 -> 'a3 -> 'a2 -> 'b)
  val c213 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a2 -> 'a1 -> 'a3 -> 'b)
  val c231 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a2 -> 'a3 -> 'a1 -> 'b)
  val c312 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a3 -> 'a1 -> 'a2 -> 'b)
  val c321 : ('a1 -> 'a2 -> 'a3 -> 'b) ->
             ('a3 -> 'a2 -> 'a1 -> 'b)

  val c1243 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a2 -> 'a4 -> 'a3 -> 'b)
  val c1324 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a3 -> 'a2 -> 'a4 -> 'b)
  val c1342 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a3 -> 'a4 -> 'a2 -> 'b)
  val c1423 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a4 -> 'a2 -> 'a3 -> 'b)
  val c1432 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a1 -> 'a4 -> 'a3 -> 'a2 -> 'b)
  val c2134 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a1 -> 'a3 -> 'a4 -> 'b)
  val c2143 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a1 -> 'a4 -> 'a3 -> 'b)
  val c2314 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a3 -> 'a1 -> 'a4 -> 'b)
  val c2341 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a3 -> 'a4 -> 'a1 -> 'b)
  val c2413 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a4 -> 'a1 -> 'a3 -> 'b)
  val c2431 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a2 -> 'a4 -> 'a3 -> 'a1 -> 'b)
  val c3124 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a1 -> 'a2 -> 'a4 -> 'b)
  val c3142 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a1 -> 'a4 -> 'a2 -> 'b)
  val c3214 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a2 -> 'a1 -> 'a4 -> 'b)
  val c3241 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a2 -> 'a4 -> 'a1 -> 'b)
  val c3412 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a4 -> 'a1 -> 'a2 -> 'b)
  val c3421 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a3 -> 'a4 -> 'a2 -> 'a1 -> 'b)
  val c4123 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a1 -> 'a2 -> 'a3 -> 'b)
  val c4132 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a1 -> 'a3 -> 'a2 -> 'b)
  val c4213 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a2 -> 'a1 -> 'a3 -> 'b)
  val c4231 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a2 -> 'a3 -> 'a1 -> 'b)
  val c4312 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a3 -> 'a1 -> 'a2 -> 'b)
  val c4321 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
              ('a4 -> 'a3 -> 'a2 -> 'a1 -> 'b)


  (* useful combinators *)
  val I : 'a -> 'a
  val K : 'a -> 'b -> 'a

  (* for fold, etc. *)
  val opandalso : bool * bool -> bool
  val oporelse  : bool * bool -> bool

  (* oneshot is a ref that can be set only once *)
  structure Oneshot :
      sig
          (* holding a value of type 'a *)
          type 'a oneshot
          (* create an uninitialized oneshot *)
          val oneshot     : unit -> 'a oneshot
          val init        : 'a   -> 'a oneshot
          val set         : 'a oneshot * 'a -> unit
          val deref       : 'a oneshot -> 'a option
          val eq          : 'a oneshot * 'a oneshot -> bool
          val wrap        : ('a -> 'a) -> 'a oneshot -> unit
      end

end
