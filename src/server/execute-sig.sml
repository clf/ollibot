
signature EXECUTE =
sig

  exception Execute of string

  type instance

  (* Create a new instance from a parsed program *)
  val new : Bytecode.program -> instance
  val destroy : instance -> unit

  (* returns true if progress was made *)
  val step : instance -> bool

  (* If there are messages to send to the client, get the first one. *)
  val message : instance -> string option

  (* Receive a message from the client, unmarshal and run it *)
  val come : instance -> string -> unit

end
