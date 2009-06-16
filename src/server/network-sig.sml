
(* This is our convenient interface to the network. It's implemented on top of
   the RAW_NETWORK signature, so if you need to do anything fancy, see that. 

   This interface relies on an event loop model. For instance, when you send to
   a socket, the data is merely placed on a queue -- when 'wait' is called,
   if that socket can accept data, we send as much as we can without blocking.
   This has the advantage that the operations like 'send' can never block, but
   forces the user of the structure to continually call the 'wait' function to
   make sure that the program makes progress.

   Each socket created has a protocol, which is used to packetize the byte
   stream received and to serialize packets for sending.

*)

signature NETWORK =
sig
    (* usually Time.time, but for test conditions, can be anything *)
    type time

    exception Network of string

    (* Conceptually, a sock is:
       
       the underlying system socket,
       a queue of data waiting to be sent out
       a queue of packets waiting to be returned as events
       an input packet that has not yet been finished,
       an indicator for the status of the socket (closed, etc.)
       a unique identifier (used for socket identity)
       a way to convert from its idea of a packet to a string,
          and vice versa

       ... but they are held abstract so you don't need to worry
       yourself with that stuff!

       *)

    type sock 
    type listener

    (* an 'a protocol sends messages of type 'a *)
    type 'a protocol

    val protocol :
      { parse : 'partialpacket -> string -> 'packet list * 'partialpacket,
        make : 'packet -> string,
        empty : 'partialpacket,
        desc : string } -> 'packet protocol

    (* must be decoded with the right protocol. *)
    type packet

    val decode : 'a protocol -> packet -> 'a
    val encode : 'a protocol -> 'a -> packet

    (* may raise RawNetwork *)
    val localhost : unit -> string
    val localhostip : unit -> int * int * int * int
    val iptostring : int * int * int * int -> string

    (* getip dns
       XXX confusing spec??
       resolve ip of host dns, if possible *)
    val lookupdns : string -> string option

      (*
    (* get the address of a peer. Only call
       this on a socket that has successfully
       connected. Anything accepted from a
       listener will be connected immediately. *)
    val getpeerip : sock -> int * int * int * int
      *)

    (* eq (s1, s2)
       true iff s1 is the same socket as s2. *)
  
    val eq : sock * sock -> bool
    val leq : listener * listener -> bool

    val compare : sock * sock -> order
    val lcompare : listener * listener -> order

    val tostring : sock -> string
    val ltostring : listener -> string

    (* do initialization. Call this before calling 'wait'. *)
    val init : unit -> unit

    (* connect prot hostname port

       returns a new socket that's waiting to connect to
       hostname:port. As soon as it is, wait will return
       a Connected event. If connection fails, wait will
       return a Closed event. connect may also raise
       an exception immediately. May raise Network. *)
    val connect : 'a protocol -> string -> int -> sock

    (* connect filename

       connects to a local UNIX domain socket. This happens
       immediately. *)
    val usconnect : 'a protocol -> string -> sock

    (* disconnect s 

       Waits for any pending data to be written, then disconnects the
       socket s. This operation does not block. You should not use the
       socket s after calling this function. *)
    val disconnect : sock -> unit
    (* May raise Network. *)
    val ldisconnect : listener -> unit

    (* listen p

       create a listener (a socket that receives connections) at port
       p. Can raise Network if the port is in use. *)
    val listen : 'a protocol -> int -> listener

    (* listen filename

       create a local UNIX domain socket and listen there.
       May raise Network. *)
    val uslisten : 'a protocol -> string -> listener

    (* when you send, you get a reference to a notify object back, which
       will be Queued until the packet is actually sent over the wire
       (at time t) and then Sent t after that. If the socket has an
       error so that the packet is not sent, it may instead become
       Failed t for some t. *)

    datatype notify =
      NQueued
    | NSent of time
    | NFailed of time

    (* send s msg

       sends the packet msg to the socket s without blocking. The
       actual transmission may not occur until 'wait' is called, or
       indeed until it is called many times. May raise Network. 

       Optionally, get back a notify. *)

    val send : sock -> packet -> unit
    val sendex : sock -> packet -> notify ref

    (* sendraw s msg

       sends a string to the socket (after any packets waiting)
       without packetizing it. sometimes it's more efficient to avoid
       the packetizing overhead (which may involve crawling over the
       data to escape it), or may be necessary because the protocol is
       not totally uniform (ie., HTTP). May raise Network. *)

    val sendraw : sock -> string -> unit
    val sendrawex : sock -> string -> notify ref

    (* sockname s name

       set the name of a socket, which is only used for debugging. *)
    val sockname : sock -> string -> unit


    type address
    (* as dotted quartet *)
    val atos : address -> string
    val addressinet : address -> bool
    val addressip : address -> int * int * int * int

    (* Sockevents are the possible results of a call to wait.
       Packet (msg, sock)  --  returned if 'sock' has a complete packet 'msg' ready.
       Closed (s)  --  returned when 's' has closed (possibly unexpectedly)
                       after this message, the sock shouldn't be used any more.
       Timeout  --  returned if the wait call timed out, or received a signal
       Accept (l, s, host, p)  --  returned if the listening server 'l'
                                   accepted a connection from 'host' at remote port
                                   'p' as socket 's'.
       *)

    datatype sockevent =
        Packet of packet * sock
      | Closed of sock
      | Connected of sock
      | Accept of listener * sock * address * int
      | Timeout

    (* wait servers sockets time

       This is the main event handler for our network -- the loop should call this.
       It blocks for up to 'time' (or forever, if NONE), waiting for something to happen:
           A packet may be completed on one of the socks passed in.
           A socket may become ready for writing, so we send some pending data.
           A socket may have an error and be closed.
           We may accept a new connection from one of our listeners.

       As soon as one of these things happens, we return a SINGLE event. (Other
       events, though handled immediately, are queued and will arrive in successive 
       calls to wait.) May raise Network.

       *)

    val wait : listener list -> sock list -> time option -> sockevent

    (* XXX: add 'shutdown', which could wait until all zombies are dead,
       or until a timeout has expired. We can do this at the end of a
       program rather than just exiting. *)
    
    (* May raise RawNetwork. *)
    val pushpackets : sock list -> unit

    (* May raise RawNetwork. *)
    val cleanup: unit -> unit

end
