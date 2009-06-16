
signature TRIVIALDB =
sig

  exception TrivialDB of string
  exception Reentrant
  (* initialize the database from the disk (erases all values/hooks) *)
  val init : unit -> unit

  (* read a key. all keys initially have the empty string as a value *)
  val read : string -> string

  (* update a key. to hold a new string value *)
  val update : string -> string -> unit
  (* add a hook to a key, that will be called when the key is updated. 
     if the hook returns false, it will be removed from the hooks list.
     hooks may not update keys or add other hooks before returning. *)
  val addhook : string -> (unit -> bool) -> unit

end
