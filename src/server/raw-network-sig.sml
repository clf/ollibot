
(* This is intended to be the only system-dependent portion of the code -- it 
   encapsulates the interface to raw unix-like sockets. Our more abstract and
   convenient network layer is provided in NETWORK. This interface is centered
   around a non-blocking framework using select for synchronization. *)

signature RAW_NETWORK =
sig

    exception RawNetwork of string

    (* type of socket descriptors. These are common to active
       connection-oriented (TCP), connectionless (UDP) and passive
       (ie., server ports) sockets for both the internet and the
       UNIX domain. If you call the functions on the wrong types of
       things, the underlying OS will return an error. *)
    type sdesc

    (* equality and comparison for sdescs. 
       Note that when a socket is closed (or experiences an error),
       that socket value may later "come alive" again as a new socket.
       Therefore, the identity of sockets is a little fishy: two sockets
       created at different times may become equal.
       *)
    val seq : sdesc * sdesc -> bool
    val scompare : sdesc * sdesc -> order

    val tostring : sdesc -> string

    type address
    (* create inet address *)
    val ipaddress : (int * int * int * int) -> address
    (* create Unix address *)
    val pathaddress : string -> address
    (* is it inet? *)
    val addressinet : address -> bool
    val addressip : address -> int * int * int * int


    (* XXX these are now redundant with 'address' *)
    (* may raise RawNetwork *)
    val localhost : unit -> string
    val localhostip : unit -> int * int * int * int

    val opensocks : unit -> int

    (* connect host port
       
       begins connecting to a remote host. This does not block. 
       The socket will select as writable as soon as the operation
       completes, though reading or writing to it will fail if
       the connection failed. May raise RawNetwork.
       *)
    val connect : string -> int -> sdesc

    (* usconnect path

       connects to a local UNIX domain socket. May raise RawNetwork. *)
    val usconnect : string -> sdesc

    (* listen port

       listens for incoming connections on some port. Call accept to
       actually begin the connection. May raise RawNetwork. *)
    val listen : int -> sdesc

    (* uslisten path

       creates a unix domain socket at the specified path. Call accept
       to wait for connections there. May raise RawNetwork. *)
    val uslisten : string -> sdesc

    (* accept s

       block until someone connects to the listening socket s. Return
       the new peer socket and its address.

       raises RawNetwork if accept fails or address cannot be resolved. *)
    val accept : sdesc -> sdesc * address

    (* disconnect a socket.

       raises RawNetwork if socket is already closed *)
    val hangup : sdesc -> unit

      (*
    (* get the address of the host on the other end
       of this socket.

       raises RawNetwork if the socket is not connected, or
       is a listener. *)
    val getpeerip : sdesc -> int * int * int * int
*)


    (* recv s 

       blocks until it can read some data from the socket s. returns
       "" if the socket has disconnected. *)
    val recv : sdesc -> string

    (* send s msg 

       sends some of msg to the socket s. Does not block. The integer
       returned indicates how many bytes were sent (perhaps none). ~1
       is returned if the socket is disconnected. *)
    val send : sdesc -> string -> int

    (* haserror sock *)
    val haserror : sdesc -> bool

    (* select (rs, ws, timeout) 

       blocks until there is activity on rs (any have data to be
       read), ws (any are ready to send data without blocking), or
       timeout is exceeded. If the timeout is NONE, the select call
       may not return. Returns the (r, w, e): the list of sdescs ready
       for reading (r), ready for writing (w), and those that have had
       exceptions (ie., they have closed) (e).
       May raise RawNetwork. *)
    val select : sdesc list * sdesc list * Time.time option ->
                   sdesc list * sdesc list * sdesc list

    (* For debugging; check to make sure sockets close on exit *)
    val test : sdesc -> bool

end
