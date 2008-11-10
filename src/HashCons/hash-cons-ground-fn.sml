(* hash-cons-ground-fn.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Functor for defining hashed-cons representation of ground terms.
 *)

functor HashConsGroundFn (T : HASH_KEY) : sig

    type hash_key = T.hash_key
    type obj = hash_key HashCons.obj

    val mk : hash_key -> obj

  end = struct

    structure HC = HashCons

    type hash_key = T.hash_key
    type obj = hash_key HC.obj

    val tbl = HC.new {eq = T.sameKey}

    val cons = HC.cons0 tbl

    fun mk term = cons(T.hashVal term, term)

  end
