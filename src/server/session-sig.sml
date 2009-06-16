(* A session is a running instance of a program.
   Each session has a unique integer that identifies it.
   
   *)

signature SESSION =
sig

  type session

  (* A session is no longer valid *)
  exception Expired
  exception Session of string

  val id         : session -> int
  val getsession : int -> session option

  (* get all the active sockets. this list can
     be invalidated by the other functions below. *)
  val sockets    : unit -> Network.sock list

  (* create a new session, running the program given by name
     and sending the initial data over the given socket *)
  val new        : Network.sock -> string -> unit

  (* an incoming socket for pushing or pulling at the supplied session id *)
  val toclient       : Network.sock -> int -> unit
  val toserver       : Network.sock -> int -> unit

  (* network events *)
  val closed     : Network.sock -> unit
  val packet     : Http.packet * Network.sock -> unit

  (* return a page that shows all the available demos with their source code *)
  val demos      : Network.sock -> unit
  (* show the file if it lives in the code path *)
  val source     : Network.sock -> string -> unit
  (* a file in the static data directory *)
  val static     : Network.sock -> string -> unit

  val favicon    : Network.sock -> unit

  (* do work if desired. true if some progress was made *)
  val step       : unit -> bool

end
