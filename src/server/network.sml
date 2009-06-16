
(* see network-sig.sml for a description. *)
(* Branched from ConCert Conductor on 22 Jun 2007. *)

structure Network :> NETWORK where type time = Time.time =
struct

    structure Q = Queue
    structure R = RawNetwork
    structure NHDB = NetHostDB
    exception Network of string

    type time = Time.time

    val debug = Params.flag false (SOME ("-debug",
                                         "Turn on debugging.")) "debug"
        
    fun debugdo f = if !debug then f() else ()
    fun dprintl s = debugdo (fn () => print ("[NET] " ^ s ^ "\n"))

    val debugnet = Params.flag false (SOME ("-debugnet",
                                            "Turn on debugging for the network layer.")) 
                                     "debugnet"
        
    fun ndebugdo f = if !debugnet then f() else ()
    fun ndprintl s = ndebugdo (fn () => (print ("[" ^
                                                SysWord.toString (Posix.Process.pidToWord 
                                                                  (Posix.ProcEnv.getpid ())) ^
                                                " (NET)] " ^ s ^ "\n");
                                       TextIO.flushOut TextIO.stdOut))

    val bytespersec' = Params.param "0"
        (SOME("-bytespersec",
        "The maximum number of outgoing packets allowed per second or zero for unlimited."))
        "bytespersec"
    val bytespersec = ref (NONE : int option) (* initialized below *)

    val connectionspersec' = Params.param "8"
        (SOME("-connectionspersec",
        "The maximum number of outgoing connection attempts allowed per second."))
        "connectionspersec"
    val connectionspersec = ref 0 (* initialized below *)

    val bytessent = ref 0

    val lastsecond = ref (Time.fromSeconds 0)

    val maxsocks = 500

    type address = R.address

    fun atos a =
      if R.addressinet a
      then 
        let val (aa, bb, cc, dd) = R.addressip a
        in
          StringUtil.delimit "." (map Int.toString [aa, bb, cc, dd])
        end
      else "(unix)"

    val addressip = R.addressip
    val addressinet = R.addressinet


    datatype notify =
      NQueued
    | NSent of Time.time
    | NFailed of Time.time

    type why = string list

    datatype status = 
        (* normal operation *)
        OK
        (* hasn't connected to remote host yet *)
      | Connecting
        (* socket has closed, but user not notified yet.
           It's not an error to use this socket --
           sends are ignored, one disconnect (or the
           receipt of the Closed event) sends it to
           Closed state. *)
      | Didclose of why
        (* the user knows the socket is closed.
           if bool is false:
                 the user has requested that the socket be closed,
                 but there's still more data to be sent (It is a zombie).
           if bool is true:
                 the socket is actually closed. *)
      | Notified of bool * why

    fun whytos w = StringUtil.delimit "+" w

    fun sttos OK = "OK"
      | sttos Connecting = "Connecting"
      | sttos (Didclose w) = "Didclose:" ^ whytos w
      | sttos (Notified (b, w)) = "(Notified " ^ Bool.toString b ^ ":" ^ whytos w ^ ")"

    (* might differ across sockets *)
    type packet = exn
    type partialpacket = exn

    datatype packetizer = P of
        { parse : partialpacket -> string -> packet list * partialpacket,
          make : packet -> string,
          empty : partialpacket,
          description : string }

    datatype 'a protocol = R of
        { pack : packetizer,
          encode : 'a -> exn,
          decode : exn -> 'a } 

    val itos = Int.toString
    fun iotos NONE = "NONE"
      | iotos (SOME i) = itos i

    fun encode (R{encode, ...}) = encode
    fun decode (R{decode, ...}) = decode

    val localhost = R.localhost

    val localhostip = R.localhostip

    fun iptostring (i1, i2, i3, i4) =
        (Int.toString i1) ^ "." ^ (Int.toString i2) ^ "."
        ^ (Int.toString i3) ^ "." ^ (Int.toString i4)

    fun lookupdns dns =
        Option.map (NHDB.toString o NHDB.addr) (NHDB.getByName dns)

    (* I don't want sockets with different protocol types to have
       different types, because I need to put them in monomorphic
       lists (Network.wait). On the other hand, I don't want to
       define a datatype here of all possible protocol types,
       because then the Network structure suffers in reusability.
       
       I use ML's "extensible datatype" mechanism, ie, exn. Upon
       registering a protocol, I package up the constructors and
       destructors -- those have type 'a -> exn and exn -> 'a.
       Now all packets and partialpackets have the type 'exn',
       and dynamic checks ensure that I don't do any bad coercions.

       Another way to think of this is that I am using exn
       like Java's Object class, and providing functions that
       do the up-coercion and down-cast.

       This complication is hidden from the user except in the
       case that his decode function fails!
       *)

    fun ('packet, 'partialpacket) protocol 
        { parse, make, empty, desc } =
        let
            exception ptag of 'packet
            and pptag of 'partialpacket

            fun decode (ptag x) = x
              | decode _ = raise Network ("wrong packet type: " ^ desc)

            fun decodepp (pptag x) = x
              | decodepp _ = raise Network ("wrong partialpacket type: " ^ desc)

            fun eparse pp s =
                let val (pl, npp) = parse (decodepp pp) s
                in (map ptag pl, pptag npp)
                end

            fun emake p = make (decode p)
        in
            R{pack = P{parse = eparse,
                       make = emake, 
                       empty = pptag empty, 
                       description = desc},
              decode = decode,
              encode = ptag}
        end

    datatype event =
        Pkt of packet
      | Cnt
      | Cls

    fun etos (Pkt p) = "Pkt ?"
      | etos Cnt = "Cnt"
      | etos Cls = "Cls"

    (* Things that can be queued to be sent. Most are just data. But we
       can also put checkpoint functions that are invoked (these
       record the time at which we reach queue positions, used to
       implement notifies). The checkpoint function is passed 'true'
       in the normal case. If a socket dies, we empty out its queue by
       discarding data and calling each checkpoint on 'false'. *)
    datatype sendable =
        Data of string
      | Checkpoint of bool -> unit

    datatype sock' = S of
        { sd : R.sdesc,
          stamp : Stamp.stamp,

          pack : packetizer,

          name : string ref,

          (* strings waiting to be sent on the network. because
             there are checkpoints sitting in the queue, we should
             never discard an outq without calling its checkpoints
             first. *)
          outq : sendable Q.queue ref,
          (* XXX better to replace these with real queue data
             structures *)
          (* received packets (or close messages) waiting to be returned as
             events. *)
                  (* push *)    (* pop *)
          evtq : (event list * event list) ref,
          (* an incomplete packet that we've received *)
          inp : partialpacket ref,
          status : status ref }

    datatype sock'' = Socket of sock'
                    | Delayed of int

    type sock = sock'' ref

    (* We also keep a queue of sockets waiting to open, separate from the operating system's
       queue. *)
    val delayedsocks = ref (Q.empty ()) : 
      ((unit -> sock') * (sock' -> unit) ref * sock) Q.queue ref

    (* PERF this would be more efficient if we used (Delayed thunks) as the arm
       of sock'' and then just acted on these functions directly.
        - tom *)
    val dsockctr = ref 0

    fun delayapp f (i : sock) =
      Q.app
        (fn (_,g,j) =>
          if i = j
          then
            let
              val g' = !g
            in
              g := (fn s => (g' s; f s))
            end
          else
            ())
        (!delayedsocks)

    (* we don't handle failure of listeners gracefully, because there's
       really no such thing as a listener failure in normal operation. *)
    datatype listener = L of
        { sd : R.sdesc,

          name : string ref,

          (* not used for listener, but copied to connected peer sockets *)
          pack : packetizer,
          stamp : Stamp.stamp,
          (* accepted peers waiting to be returned as events *)
                   (* push *)                     (* pop *)
          evtq : ((sock' * R.address * int) list * (sock' * R.address * int) list) ref }

    fun qlength (ref(a, b)) = length a + length b

    fun ltos (L{sd, name, stamp, evtq, pack=P{description,...}}) = 
        "([" ^ description ^ "/" ^ !name ^ "] lsd: " ^  R.tostring sd ^ " " ^ 
        Stamp.tostring stamp ^ " evtq: " ^ Int.toString (qlength evtq) ^ ")"
        
    fun fix num s = StringUtil.harden (StringUtil.charspec "-|{}[]:;<>?!@~`#%^&*_+='/.,\\\034 ") #"$" num s 

    fun socktos' (S{name, sd, stamp, outq, evtq=ref(eq1,eq2), 
                   inp, status=ref status, pack=P{description,...}}) =
        "([" ^ description ^ "/" ^ !name ^ "] sd: " ^ R.tostring sd ^ " " ^ 
        Stamp.tostring stamp ^ " out: "
        ^ StringUtil.delimit "," (map (fn Data s => "[" ^ fix 30 s ^ "]"
                                       | _ => "??" ) (Q.tolist (!outq))) ^
        " evt: " ^ StringUtil.delimit "," (map etos (eq1 @ rev eq2)) ^
        " inp: ? status: " ^ sttos status ^ ")"

    fun socktos (ref (Socket s)) = socktos' s
      | socktos (ref (Delayed x)) = ("Delayed " ^ Int.toString x)

    fun sockname' (S{name, ...}) str = name := str
    fun sockname (ref (Socket s)) str = sockname' s str
      | sockname i str = (ndprintl "Delaying sockname."; delayapp (fn s => sockname' s str) i)

(*
    fun getpeerip (ref (Socket (S {sd, ...}))) =
      XXX HERE
      | getpeerip _ = raise Network "getpeerip: socket not ready"
*)

    datatype sockevent =
        Packet of packet * sock
      | Closed of sock
      | Connected of sock
      | Accept of listener * sock * address * int
      | Timeout

    datatype sockevent' =
        Packet' of packet * sock'
      | Closed' of sock'
      | Connected' of sock'
      | Accept' of listener * sock' * address * int
      | Timeout'

    fun setos Timeout' = "Timeout"
      | setos (Accept' (l, s, h, p)) = "Accept (?, " ^
                                       socktos' s ^ ", " ^ atos h ^ ", " ^
                                       Int.toString p ^ ")"
      | setos (Closed' s) = "Closed " ^ socktos' s
      | setos (Connected' s) = "Connected " ^ socktos' s
      | setos (Packet' (p,s)) = "Packet (?, " ^ socktos' s ^ ")"


    fun eq' (S{stamp=a, ...}, S{stamp=b, ...}) = Stamp.eq (a, b)
    fun eq (ref (Socket a), ref (Socket b)) = eq' (a,b)
      | eq (ref (Delayed i), ref (Delayed j)) = i = j
      | eq _ = false
    fun compare' (S{stamp=a, ...}, S{stamp=b, ...}) = Stamp.compare(a, b)
    fun compare (ref (Socket x), ref (Socket y)) = compare' (x,y)
      | compare (ref (Delayed x), ref (Delayed y)) =
        if x < y then LESS else if x = y then EQUAL else GREATER
      | compare (ref (Delayed _), ref (Socket _)) = GREATER
      | compare (ref (Socket _), ref (Delayed _)) = LESS

    fun leq (L{stamp=a, ...}, L{stamp=b, ...}) = Stamp.eq (a, b)
    fun lcompare (L{stamp=a, ...}, L{stamp=b, ...}) = Stamp.compare(a, b)

    (* We keep around a list of zombies. These are sockets that have
       been disconnected, but that haven't yet sent all of their data.
       When outq becomes nil, close them, and drop them from this list
       so that they may be garbage collected. *)
    val zombies = ref nil : sock' list ref

    fun push (r as (ref (a, b))) x = r := (x :: a, b)
    fun hsup (r as (ref (a, b))) x = r := (a, x :: b)
    fun pop (r as (ref (a, x::b))) = (r := (a, b); x)
      | pop (r as (ref (nil, nil))) = raise Network "queue empty!"
      | pop (r as (ref (a, nil))) = (r := (nil, rev a); pop r)

    (* XXX add protocols to these. *)
    fun connect' (R{pack=pack as P{empty,...}, ...}) h p =
        let val s = Stamp.new ()
        in
            ndprintl ("Connecting(ip) to " ^ h ^ ":" ^ Int.toString p ^ 
                      " with stamp " ^ Stamp.tostring s);
            S{ sd = R.connect h p,
               name = ref ("connect " ^ h ^ ":" ^ Int.toString p),
               stamp = s,
               pack = pack,
               outq = ref (Q.empty()),
               evtq = ref (nil, nil),
               inp = ref empty,
               status = ref Connecting } 
            handle R.RawNetwork s => raise Network s
        end

    fun connect r h p =
        let
          val sr = ref (Delayed (!dsockctr))
        in
          (dsockctr := (!dsockctr + 1 handle Overflow => 0);
          delayedsocks := Q.enq ((fn () => connect' r h p, ref (fn s => ()), sr), !delayedsocks);
          sr)
        end

    fun usconnect' (R{pack=pack as P{empty,...}, ...}) p =
        S{ sd = R.usconnect p,
           pack = pack,
           name = ref ("usconnect " ^ p),
           stamp = Stamp.new (),
           outq = ref (Q.empty()),
           evtq = ref (nil, nil),
           inp = ref empty,
           status = ref OK } handle R.RawNetwork s => raise Network s

    (* Unix sockets shouldn't have to be delayed *)
    fun usconnect r p = ref (Socket (usconnect' r p))

    fun listen (R{pack, ...}) p =
        L{ sd = R.listen p,
           name = ref ("inet:" ^ Int.toString p),
           pack = pack,
           stamp = Stamp.new (),
           evtq = ref (nil, nil) } handle R.RawNetwork s => raise Network s

    fun uslisten (R{pack, ...}) p =
        L{ sd = R.uslisten p,
           name = ref ("ux: " ^ p),
           pack = pack,
           stamp = Stamp.new (),
           evtq = ref (nil, nil) } handle R.RawNetwork s => raise Network s

    (* just move to zombie list. *)
    fun disconnect' (s as S{status = ref (Notified _), ...}) = 
        let in
            raise Network "sock already closed (disconnect)"
        end
      | disconnect' (S{status = r as ref (Didclose w), ...}) = r := Notified (true, "disconnect" :: w)
      | disconnect' (s as S{status, ...}) =
        let in
            ndprintl ("making " ^ socktos' s ^ " into a zombie.");
            zombies := s :: !zombies;
            status := Notified (false, ["disconnect."])
        end

    fun disconnect (ref (Socket s)) = disconnect' s
      | disconnect i = (ndprintl "Delaying disconnect"; delayapp (fn s => disconnect' s) i)

    fun ldisconnect (L{sd,...}) = R.hangup sd handle R.RawNetwork s => raise Network s

    (* apply a function to a socket, now or later *)
    fun sapp f (ref (Socket s)) = f s
      | sapp f (ds : sock) = (ndprintl "Delaying application"; delayapp f ds)

    (* ok to send while connecting, even if the socket is on our delayed
       queue *)
    (* these versions get actual sockets, as well as a checkpoint fn to
       enqueue as well *)
    fun sendraw' _ (s as S{status = ref (Notified _), ...}) _ = 
        let in
            raise Network ("socket is closed (sendraw): " ^ socktos' s)
        end
      (* tried to send on a socket that's closed, but the client doesn't
         know that yet *)
      | sendraw' ck (s as S{status = ref (Didclose _), ...}) _ = 
          (* fail notify immediately, since it's already closed *)
        let in
          ndprintl ("Failing to enqueue packet in" ^ socktos' s);
          ck false
        end
      | sendraw' ck (s as S{outq, ...}) str = 
        let in
          ndprintl ("Enqueuing packet in" ^ socktos' s);
          outq := Q.enq (Data str, !outq);
          outq := Q.enq (Checkpoint ck, !outq)
        end

    fun send' ck (s as S{pack=P{make,...},...}) p = sendraw' ck s (make p)

    (* possibly delayed versions of the above. *)
    fun sendrawex sk p =
      let
        val r = ref NQueued
        fun ck true = r := NSent (Time.now ())
          | ck false = r := NFailed (Time.now ())
      in
        ndprintl "Sapping sendraw'";
        sapp (fn s => sendraw' ck s p) sk;
        r
      end

    fun sendex sk p = 
      let
        val r = ref NQueued
        fun ck true = r := NSent (Time.now ())
          | ck false = r := NFailed (Time.now ())
      in
        ndprintl ("Sapping send'.");
        sapp (fn s => (ndprintl ("Preparing to enqueue data in " ^ socktos' s); send' ck s p)) sk;
        r
      end

    (* PERF could avoid inserting checkpoints here *)
    fun send sk p = ignore (sendex sk p)
    fun sendraw sk p = ignore (sendrawex sk p)

    fun pop_data outq =
      case Q.deq (!outq) of
        (SOME (Data s), q) => (outq := q; s)
      | (SOME (Checkpoint c), _) => raise Network ("outq invt violation: " ^
                                                   "head of queue is checkpoint, not data")
      | _ => raise Network ("outq invt violation: " ^
                            "queue is empty!")

    fun execute_checkpoints outq =
      case Q.deq (!outq) of
        (SOME (Data s), q) => () (* keep old queue *)
      | (SOME (Checkpoint c), q) => (c true; outq := q; execute_checkpoints outq)
      | _ => ()

    fun fail_outq outq =
      let
        fun fo q =
          case Q.deq q of
            (SOME (Data _), q) => fo q (* ignore data *)
          | (SOME (Checkpoint c), q) => (c false; fo q)
          | _ => ()
      in
        fo (!outq);
        outq := (Q.empty())
      end

    fun wait' lis soc (to : Time.time option) =
        let
            (*val _ = app
              (fn (S{sd=sd,...}) => (if R.test sd then print "Socket ok\n"  else print "SOCKET BAD!\n"))
              (soc)*)

            val _ = ndebugdo (fn () =>
                              ndprintl (" ( wait ) -------------- Doing wait on sockets: " ^
                                        "------------\n        " ^
                                        StringUtil.delimit "\n        "
                                        (map socktos' soc) ^ "\n     And listeners\n        " ^
                                        StringUtil.delimit "\n        "
                                        (map ltos lis)))

            val _ = ndebugdo (fn () =>
                              if null (!zombies)
                              then ndprintl ("No zombies.")
                              else ndprintl ("Zombies:\n       " ^
                                             StringUtil.delimit "\n       "
                                             (map socktos' (!zombies))))

            (* XXX are there any other sanity checks that it makes sense to do here? *)

            fun sane (s as S{status=ref (Notified _), ...}) = 
                 raise Network ("closed socket passed to select: " ^ socktos' s)
              | sane _ = ()

            (* XXX put debugdo here so we don't quit if trying to connect to a node
               that just failed (closed listener passed to select) *)
            val _ = app sane soc

            (* Check to see if any zombies have sent all their data. 
               If so, remove them from the zombies list and put them
               in Notified true state. Zombies may be in the "Notified
               false" state, by default, or in the "Didclose" state,
               if the socket was buffered and had an exception *)


            fun onezombie (s as S{sd, status=status as ref (Notified (false, w)), outq, ...}) =
              if Q.isempty (!outq) then
                let in
                    ndprintl ("zombie " ^ socktos' s ^ " discarded because outq empty");
                    status := Notified (true, "zomb_outq_empty" :: w);
                    (R.hangup sd) handle R.RawNetwork _ => ();
                    false
                end
              else true
              | onezombie (s as S{sd, status=status as ref (Didclose w), outq, ...}) =
                let in
                    ndprintl ("zombie " ^ socktos' s ^ " discarded because status Didclose");
                    status := Notified (true, "onezombie" :: w);
                    (R.hangup sd) handle R.RawNetwork _ => ();
                    fail_outq outq;
                    false
                end
              | onezombie _ = raise Network "inconsistent socket state (zombies)"

            val _ = zombies := List.filter onezombie (!zombies)

            val wsoc = (!zombies) @ soc

            fun geteventreally () =
                let
                    fun are nil = Timeout'
                      | are (L{evtq=ref (nil, nil), ...}::ll) = are ll
                      | are ((l as L{evtq, ...})::_) = let val (a, b, c) = pop evtq
                                                       in Accept' (l, a, b, c)
                                                       end

                    fun ree nil = Timeout'
                      | ree (S{evtq=ref (nil, nil), ...}::ll) = ree ll
                      | ree ((s as S{evtq, status, ...})::_) = 
                        case pop evtq of
                            Pkt p => Packet'(p, s)
                          | Cnt =>
                                let in
                                    ignore(!status = OK
                                         orelse raise 
                                         Network ("inconsistent socket state (Cnt event): " ^ 
                                                  sttos (!status)));
                                    Connected' s
                                end
                          | Cls => 
                                (* if we send a Cls packet, then
                                   that means that this socket closed,
                                   and we're now finished sending all of
                                   its packets. At this point, this socket
                                   should no longer be used. *)
                                let in
                                  (case !status of
                                     Didclose w => status := Notified(true, "Cls" :: w)
                                   | _ => raise Network "inconsistent socket state (Cls)!");
                                  Closed' s
                                end
                in
                    case are lis of
                        Timeout' => ree soc
                      | e => e
                end

            fun getevent () =
                let 
                    val e = geteventreally ()
                in
                    ndprintl ("Getevent returns event: " ^ setos e);
                    e
                end

        in
            case getevent () of
                Timeout' =>
                    let
                        (* no event sitting around. call select. *)

                        (* reads: all of the sockets, and the listeners. 
                           (But not the zombies. We don't care to read from them.)
                           *)
                        val rs = 
                            map (fn (L{sd,...}) => sd) lis @
                            map (fn (S{sd,...}) => sd) soc

                        (* writes: only the sockets with something queued,
                                   (or those waiting to connect! - tom) *)
                        val ws = List.mapPartial (fn (S{sd,status=ref Connecting, ...}) => SOME sd
                                                   | (S{sd, outq, ...}) =>
                                                      if Q.isempty (!outq) then NONE
                                                      else SOME sd) wsoc

                        val (rr, ww, ee) = R.select (rs, ws, to)
                            handle RawNetwork.RawNetwork s => (ndprintl ("select error: " ^ s);
                                                               raise Network "select error")

                        val _ = ndprintl ("R: " ^ 
                                          StringUtil.delimit "," 
                                            (map R.tostring rr) ^
                                          " W: " ^
                                          StringUtil.delimit "," 
                                            (map R.tostring ww) ^
                                          " X: " ^
                                          StringUtil.delimit "," 
                                            (map R.tostring ee))

                        (* now process these to do some events. 
                           if any socket is in the ee list, it's closed.
                           if any socket is in the ww list, 
                                send some data on it. (There's buffer space.)
                           if any socket is in the rr list and has status OK,
                                read some data from it. (There's some available.)
                                It might have just connected, so send Connected
                                event too.
                           if any listener is in the rr list,
                                accept from it. (There's a connection available.)
                           *)

                        (* close any sockets in ee. I don't even look for listeners,
                           because those shouldn't have exceptions. (??) *)

                        fun closeone s =
                            (ndprintl ("Socket exception: " ^ R.tostring s);
                             case List.find (fn S{sd, ...} => R.seq (sd, s)) wsoc of
                                NONE => raise Network "exception on non-socket (strange!)"
                              | SOME (S{sd, status=status as ref status', evtq, outq, ...}) => 
                                if case status' of
                                       OK => true
                                     | Connecting => true
                                     (* XXX loses why *)
                                     | Notified (false, _) => true
                                     | _ => false
                                then
                                let in
                                    (R.hangup sd) handle R.RawNetwork _ => ();
                                    status := Didclose ["socketexn+?"];
                                    fail_outq outq;
                                    push evtq Cls
                                end
                                else raise Network "inconsistent states internally... (closeone)")

                        val _ = app closeone ee

                        fun dorecv sd r evtq inp (P{parse,...}) outq first =
                            (let 
                                 (* receive data. "" or exception means that
                                    the connection has closed. Use parsepacket
                                    to see if any packets have been completed; if so
                                    push them. On any error, transition to Didclose
                                    state. *)
                                 val dat = R.recv sd
                                 val _ = (dat = "") andalso raise R.RawNetwork ""
                                 val (pl, pp) = parse (!inp) dat
                             in
                                 ignore (first ());
                                 inp := pp;
                                 app (fn p => push evtq (Pkt p)) pl
                             end handle R.RawNetwork _ =>
                                 let in
                                     (R.hangup sd) handle _ => ();
                                       (* XXX correct? *)
                                       fail_outq outq;
                                       r := Didclose ["exn_in_dorecv"];
                                       push evtq Cls
                                 end)

                        (* for each socket in the recv list, deal with it. *)
                        fun recvone s =
                            (ndprintl ("Readable: " ^ R.tostring s);
                             case List.find (fn S{sd, ...} => R.seq (sd, s)) soc of
                                NONE =>
                                    (* accept on a listener *)
                                    (case List.find (fn L{sd, ...} => R.seq (sd, s)) lis of
                                         NONE => 
                                                (* this is okay, if we also got an exception
                                                   for that socket. *)
                                           let 
                                             fun ensure_ex nil = 
                                               raise Network "recv event on unknown sdesc (??)"
                                               | ensure_ex (sde :: rest) =
                                                 if R.seq (sde, s) then ()
                                                 else ensure_ex rest
                                           in
                                             ensure_ex ee
                                           end
                                       | SOME (L{sd, evtq, pack = pack as P{empty, ...},...}) =>
                                             (let 
                                                  (* will happen immediately *)
                                                  val (ps, addr) = R.accept sd

                                                  val peer =
                                                      S{ sd = ps,
                                                         name = ref ("accept @" ^ R.tostring sd),
                                                         pack = pack,
                                                         stamp = Stamp.new (),
                                                         outq = ref (Q.empty ()),
                                                         evtq = ref (nil, nil),
                                                         inp = ref empty,
                                                         status = ref OK } 
                                              in
                                                  (* FIXME port ? *)
                                                  push evtq (peer, addr, 0)
                                                  (* XXX accept failed, close socket? *)
                                              end handle RawNetwork.RawNetwork _ => ()))
                              | SOME (S{sd, evtq, inp, status=r as ref OK, pack, outq, ...}) =>
                                    (dorecv sd r evtq inp pack outq (fn () => ()))
                              (* XXX is this possible? I believe it only selects writable
                                 on a connection (?) Probably better to use haserror instead. *)
                              | SOME (S{sd, evtq, inp, status=r as ref Connecting, pack, outq,  ...}) =>
                                    (dorecv sd r evtq inp pack outq (fn () => (r := OK; push evtq Cnt)))
                              | SOME _ => ((* ignore. socket is closed, don't recv. *)))

                        val _ = app recvone rr

                          (* recv done. try writable sockets now... *)

                        (* XXX can simplify type to pull fields out of 'this' *)
                        fun dosend this sd evtq status (outq : sendable Q.queue ref) =
                            if 
                               (* have we gone at least one second? *)
                                Time.>= 
                                (Time.-(Time.now(), (!lastsecond)),
                                Time.fromSeconds 1)
                            orelse
                            (* or is the packet limit greater than the
                               number we've sent so far this second?
                               (ie, we have room for more packets) *)
                            (
                                case (!bytespersec)
                                of SOME x => x > (!bytessent)
                                |  NONE => true
                            )
                            then
                            (* Send another packet *)
                                (let
                                     val _ = 
                                         ndprintl (socktos' this ^ " ready to write.")
                                         
                                     val _ = 
                                       if
                                         Time.>= 
                                         (Time.-(Time.now(), (!lastsecond)),
                                         Time.fromSeconds 1)
                                       then
                                       let in
                                         lastsecond := Time.now();
                                         bytessent := 0
                                       end
                                       else
                                         ()
                                         
                                     val d = pop_data outq
                                     val len = size d
                                     val _ = 
                                         ndprintl ("(w') Send: " ^
                                                   R.tostring sd ^
                                                   " " ^ fix 200 d)
                                     val n = R.send sd d
                                 in

                                     ignore(n = ~1 andalso raise R.RawNetwork "");
                                     bytessent := (!bytessent) + n;
                                     (if n < len
                                        (* since we're putting something back at
                                           the head of the queue, we don't execute
                                           checkpoints *)
                                      then outq := 
                                       Q.cons (Data (String.substring (d, n, len - n)),
                                               !outq)
                                       (* but if we finished the whole data packet,
                                          then we need to execute the checkpoints
                                          that follow it to maintain the invt on outq *)
                                      else execute_checkpoints outq);

                                     ndprintl ("Sent " ^ Int.toString n)
                                 end handle R.RawNetwork _ =>
                                     let in  
                                         ndprintl ("send exn..");
                                     (* if it's a zombie, then we just 
                                        throw the socket away. otherwise,
                                        put in Didclose state and push 
                                        the notification. *)
                                     
                                      if List.exists (fn (S{sd=z,...}) =>
                                                      R.seq (z, sd)) (!zombies)
                                      then
                                          let in
                                              status := Notified (true, ["dosend_exn"]);
                                              zombies := List.filter 
                                              (
                                                  fn (S{sd=z,...}) =>
                                                  (
                                                      ndprintl "filtering zombies due to RN error";
                                                      if
                                                          (not (R.seq (z, sd))) 
                                                      then
                                                          true (* keep *)
                                                      else
                                                          let in
                                  (*This function did not originally hang up when
                                    throwing away zombies.  This was causing
                                    hundreds of open file descriptors.  Hanging
                                    up fixes this problem, but I'm not sure it's
                                    the proper thing to do.
                                    - mkehrt *)
                                                              fail_outq outq;
                                                              R.hangup z;
                                                              false
                                                          end
                                              ))
                                              (!zombies)
                                          end
                                      else
                                          let in
                                              ndprintl ("Closed due to write err: " ^ 
                                                        socktos' this);
                                              fail_outq outq;
                                              status := Didclose ["write_err"];
                                              push evtq Cls
                                          end
                                     end)
                            else
                            (* We've sent our packet limit this second *)
                              ndebugdo (fn () =>
                                        let in
                                          print "Throttled! ";
                                          print ("now: " ^ (Time.toString (Time.now())) ^
                                                 " last: " ^ (Time.toString (Time.now())) ^
                                                 " bytespersec: " ^ iotos (!bytespersec) ^ 
                                                 " bytessent: " ^ itos (!bytessent) ^ "\n")
                                        end)
                                                         
                                 
                        (* we know outq will be non-empty *)
                        fun writeone (s:R.sdesc) =
                            ((* ndprintl ("Writable: " ^ R.tostring s); *)
                             case List.find (fn S{sd, ...} => R.seq (sd, s)) wsoc of
                                 (* we may have closed it in recv, even though it was
                                    writable *)
                                 SOME (S{status=ref (Didclose _), ...}) => ()
                               | SOME (this as S{sd, evtq, 
                                                 outq, status=r as ref Connecting, ...}) =>
                                 if R.haserror sd
                                 then (ndprintl (socktos' this ^ " failed to connect.");
                                       r := Didclose ["failed_connect"];
                                       push evtq Cls)
                                 else (r := OK; 
                                       push evtq Cnt)
                               | SOME (this as S{sd, evtq, status, outq,...}) =>
                                   if Q.isempty (!outq)
                                   then 
                                     (* a connecting socket can be both read-
                                        and write-enabled, so it might be set OK
                                        in readone, and then get here for writeone. 
                                        Just ignore it. *)
                                     ()
                                   else (dosend this sd evtq status outq)
                               | NONE => raise Network ("Bug: writable socket " ^ 
                                                        R.tostring s ^ 
                                                        " not in interesting list!"))

                        val _ = app writeone ww

                    in
                        getevent ()
                    end
              | e => e
 
        end

    fun heartbeat () = let
        fun activate n =
          if
            (not (Q.isempty (!delayedsocks))) andalso
            R.opensocks () < maxsocks
            andalso n < (!connectionspersec)
          then
            let
              val (d,q) = Q.deq (!delayedsocks)
              val (f,g,s) = valOf d
              val sck = f ()
              val _ = ndprintl ("Connected delayed sock " ^ socktos' sck)
            in
              delayedsocks := q;
              (!g) sck;
              s := Socket sck;
              activate (n + 1)
            end handle _ => ()
          else
            ()
      in
        activate 0
      end

    fun convert (Packet' (p,s)) = Packet (p, ref (Socket s))
      | convert (Closed' s) = Closed (ref (Socket s))
      | convert (Connected' s) =
          Connected (ref (Socket s))
      | convert (Accept' (l,s,t,i)) = 
          Accept (l, ref (Socket s), t, i)
      | convert Timeout' = Timeout

    fun wait ll sl to =
      let in
        heartbeat ();
        convert (wait' ll (foldr (fn (ref (Socket x), l) => x::l | (_,l) => l) [] sl) to)
      end

    val tostring = socktos
    val ltostring = ltos


    (*
      loops through the given socket list and sends all pending packets
      used for cleaning up

      XXX this appears to copy a lot of code from above; can we
      merge them? - tom

      XXX throttle send/connect speed
    *)
    fun pushpackets' sl  =
        let
            fun send this sd (outq : sendable Q.queue ref) =
                let
                    val _ = 
                        ndprintl (socktos' this ^ " ready to write (pp').")

                    val d = pop_data outq
                    val len = size d
                    val _ = 
                        ndprintl ("(pp') Send: " ^
                            R.tostring sd ^
                            " " ^ fix 200 d)
                    val n = R.send sd d
                in
                    ignore(n = ~1 andalso raise R.RawNetwork "");
                    if n < len 
                    (* maintains data-front invt *)
                    then outq := Q.cons (Data (String.substring (d, n, len - n)),
                                         !outq)
                    else execute_checkpoints outq
                end handle R.RawNetwork _ =>
                let in
                    (* the socket will be removed the next time,
                       since its queue is empty. *)
                    fail_outq outq
                  end

                fun loop ((z as S{sd, outq,...})::t) =
                  if Q.isempty (!outq)
                  then 
                    let in
                      (* hang up, ignoring errors *)
                      R.hangup sd handle _ => ();
                      loop t
                    end
                  else 
                    let in
                        send z sd outq;
                        loop (z::t)
                    end    
                  | loop nil = ()

        in
            loop sl
        end

    fun pushpackets sl =
        pushpackets' (List.mapPartial (fn (ref (Socket x)) => SOME x | _ => NONE) sl)

    (*
      pushes all pending packets in zombies
    *)
    fun cleanup () =
    let in
        ndprintl "cleaning up.";
        pushpackets' (!zombies)
    end

    fun init () =
      let in
        bytespersec := 
        let val x = Params.asint 0 bytespersec'
        in
          if (x = 0)
          then NONE
          else SOME x
        end;

        connectionspersec := Params.asint 8 connectionspersec'

        (* ; Cleanup.install "Network" cleanup *)
      end

end
