
signature GROWMONOARRAY =
sig

  type elem
  type monoarray

  (* imperative arrays that automatically grow to
     accommodate new data. The array can have 'holes'
     where no data are stored, though these are not
     treated efficiently. *)
  type growarray
    
  val growarray : int -> elem -> growarray
  val empty : unit -> growarray
  val init : int -> growarray

  (* return actual length *)
  val length : growarray -> int

  (* can raise Subscript when out of bounds *)
  val sub : growarray -> int -> elem

  (* update cannot grow the array; will raise subscript *)
  val update : growarray -> int -> elem -> unit

  (* stick an element at the end *)
  val append : growarray -> elem -> unit

  (* Remove characters from the end so that it is the given length.
     Raises subscript if the array is not at least this long already. *)
  val truncate : growarray -> int -> unit

  (* after calling this, don't use the growarray
     any longer, since it may share data with the returned
     array. *)
  val finalize : growarray -> monoarray

end
