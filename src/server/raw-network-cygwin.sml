
(* MLton FFI implementation of RAW_NETWORK. *)
(* raw-network-nj.sml is generated from this file and should not be edited.
   (edit raw-network.sml instead!) *)

structure RawNetwork :> RAW_NETWORK =
struct

    val debug = Params.flag false (SOME ("-debug",
                                         "Turn on debugging.")) "debug"

    fun debugdo f = if !debug then f() else ()
    fun dprintl s = debugdo (fn () => (print ("[" ^
                                              SysWord.toString (Posix.Process.pidToWord 
                                                                (Posix.ProcEnv.getpid ())) ^
                                             " RAW] " ^ s ^ "\n");
                                       TextIO.flushOut TextIO.stdOut))

    val debugraw = Params.flag false (SOME ("-debugraw",
                                     "Turn on debugging for the raw network module.")) 
                            "debugraw"
        
    fun rdebugdo f = if !debugraw then f() else ()
    fun rdprintl s = rdebugdo (fn () => (print ("[" ^
                                                SysWord.toString (Posix.Process.pidToWord 
                                                                  (Posix.ProcEnv.getpid ())) ^
                                                " (RAW)] " ^ s ^ "\n");
                                         TextIO.flushOut TextIO.stdOut)
                                         )

    val recvsize = Params.param "1024" (SOME ("-recvsize",
                                              "Size of receive buffer.")) "recvsize"

    val eagain = _import "ML_EAGAIN" : int ; 
    val pf_inet = _import "ML_PF_INET" : int ; 
    val sock_stream = _import "ML_SOCK_STREAM" : int ;

    exception RawNetwork of string

    datatype sty = Inet | Unix
    type sdesc = int * sty

    datatype address = 
        IP of int * int * int * int
      | UNIX of string

    fun ipaddress x = (IP x)
    fun pathaddress x = (UNIX x)

    fun addressip (IP a) = a
      | addressip (UNIX _) = raise RawNetwork "can't ip of local unix peer"

    val numsocks = ref 0

    (* workaround 2002 basis *)
    fun ca_prefix (arr, n) =
      let val v = CharArray.vector arr
      in
        CharVectorSlice.vector
        (CharVectorSlice.slice(v, 0, SOME n))
      end

    fun readcstring arr =
        let
            fun go n =
                if n >= CharArray.length arr then 
                   raise RawNetwork "not null-terminated"
                else 
                    case CharArray.sub (arr, n) of
                        #"\000" => ca_prefix (arr, n)
                      | _ => go (n + 1)
        in
            go 0
        end

    fun localhost () =
        let 
            val sys_gethostname = _import "gethostname" : CharArray.array * int -> int ;
            (* XXX mlton bug (does this still exist? tom 25 Apr 2006) *)
            (* val buf = Unsafe.CharArray.create 512  *)
            val buf = CharArray.array (512, #"a")
        in
            case sys_gethostname (buf, 511) of
                0 => (* some systems may not null-terminate if the host name is
                        too long *)
                     let in
                         CharArray.update(buf, 511, chr 0);
                         readcstring buf
                     end
              | _ => raise RawNetwork "can't get local hostname"
        end

    fun localhostip () =
        let
            val sys_getlocalip = _import "getlocalip" : CharArray.array -> unit;
            val ip = CharArray.array (4, #"a")
        in
            sys_getlocalip(ip);
            (ord (CharArray.sub(ip, 0)),
             ord (CharArray.sub(ip, 1)),
             ord (CharArray.sub(ip, 2)),
             ord (CharArray.sub(ip, 3)))
        end

    val seq = op =
    (* assumes socket descriptors are unique *)
    fun scompare ((a, _), (b, _)) = Int.compare (a, b)

    fun tostring (a, _) = Int.toString a

    fun geterrno () =
        let val sys_geterrno = _import "ml_get_errno" : unit -> int ;
        in  sys_geterrno ()
        end
    
    fun recv (s, _) =
        let
            val sys_recv = _import "ml_recv" : int * CharArray.array * int -> int ;
            val sz = Params.asint 1024 recvsize

            (* could create and initialize, but that is a waste here *)
            (* XXX mlton bug *)
            (* val buf = Unsafe.CharArray.create sz *)
            val buf = CharArray.array (sz, #"a")
        in
            case sys_recv (s, buf, sz) of
                ~1 => ""
              | n => ca_prefix(buf, n)
        end

    fun send (s, _) msg =
        let
            val sys_send = _import "ml_send" : int * CharVector.vector * int -> int ;

            (* val () = rdprintl ("call C") *)
            val n = sys_send (s, msg, size msg)
        in
            (* rdprintl ("back from C; n: " ^ Int.toString n); *)
            case n of
                ~1 => if geterrno () = eagain
                      then 0
                      else ~1
              | n => n
        end
   
    fun haserror (s, _) =
        let 
            val sys_haserror = _import "ml_haserror" : int -> int ;
        in
            case sys_haserror s of
                0 => false
              | _ => true
        end

    fun addressinet (IP _) = true
      | addressinet _ = false

    fun connect a p =
        let
            val sys_connect = _import "ml_connect" : string * int -> int ;
        in
            case sys_connect (a ^ "\000", p) of
                ~1 => raise RawNetwork ("connect to '" ^ a ^ ":" ^
                                        Int.toString p ^ "' failed")
              | s => (numsocks := !numsocks + 1; (s, Inet))
        end

    fun usconnect path =
        let
            val sys_connectux = _import "ml_connectux" : string -> int ;
        in
            case sys_connectux (path ^ "\000") of
                ~1 => raise RawNetwork ("usconnect to '" ^ path ^ "' failed")
              | n => (numsocks := !numsocks + 1; (n, Unix))
        end

    fun uslisten path =
        let
            val sys_uslisten = _import "ml_uslisten" : string -> int ;
        in 
            case sys_uslisten (path ^ "\000") of
                ~1 => raise RawNetwork "uslisten: unable to listen"
              | s => (numsocks := !numsocks + 1; (s, Unix))
        end

    fun hangup (sk, _) = 
        let
(*            val cl = _import "close" : int -> int ;*)
            val cl = _import "ml_close" : int -> int ;
        in
            rdprintl ("hanging up fd " ^ Int.toString sk);
            case cl sk of
                ~1 => raise RawNetwork "socket is already closed! (hangup)"
              | _ => (numsocks := !numsocks - 1)
        end

    fun listen port = 
        let
            val sys_socket = _import "socket" : int * int * int -> int ;
            val sys_listen = _import "listen" : int * int -> int ;
            val sys_bindport = _import "ml_bindport" : int * int -> int ;

            val sk = sys_socket(pf_inet, sock_stream, 0)
        in
            ignore(sys_bindport (sk, port) = 0 orelse 
                   raise RawNetwork ("can't bind port: "^(Int.toString port)));
            ignore(sys_listen (sk, 20) = 0 orelse raise RawNetwork "can't listen");
            numsocks := !numsocks + 1;
            (sk, Inet)
        end

    (* return the address (as IP) of the socket *)
    fun getpeername s =
        let
            val sys_peername = _import "ml_peername" : int * CharArray.array -> int ;
            (* X.X.X.X will never be longer than about 16 bytes *)
            (* XXX mlton bug *)
            (* val buf = Unsafe.CharArray.create 32 *)
              
            val buf = CharArray.array (32, #"a")
        in

            case sys_peername (s, buf) of
              0 => (* use mlton's built in string marshaler *)
                let val pn = readcstring buf
                in
                  rdprintl ("peername: " ^ pn);
                  (case map Int.fromString
                     (String.fields (fn #"." => true | _ => false) pn) of
                     [SOME a, SOME b, SOME c, SOME d] => (a, b, c, d)
                   | _ => raise RawNetwork "peer name is bogus")
                end
              | _ => raise RawNetwork "can't get peer name"
        end

    fun accept (l, sty) =
        let
            val sys_accept = _import "ml_accept" : int -> int ;
            val cl = _import "close" : int -> int ;
        in
            case sys_accept l of
                ~1 => raise RawNetwork "accept failed"
              | s =>
                let
                    val p = (case sty of
                               Inet => IP (getpeername s)
                             | Unix => UNIX "UNIX")
                        handle e => (ignore (cl s); raise e)
                in
                    numsocks := !numsocks + 1;
                    ((s, sty), p)
                end
        end

    fun test (fd,_) = 
      let
        val test' = _import "ml_test" : int -> int ;
      in
        case test' fd of 0 => false
           | _ => true
      end

    (* XXX we should not depend on splaymapfn for this simple thing *)
    structure M = SplayMapFn(type ord_key = int
                             val compare = Int.compare)

    fun select (ins : sdesc list, outs : sdesc list, to) =
        let
            (* XXX poll is more stable than select on Cygwin (select works on top of poll) *)
            (* we first build a map of file descriptors and the events
               (read/write) to detect. then the list is allocated with malloc
               and passed to poll with the chosen timeout.

               when poll returns, the list is examined to determine which
               sockets are ready for read/write or had exceptions. *)

            val _ = rdprintl ("select (ins: " ^
                              StringUtil.delimit "," (map tostring ins) ^
                              " outs: " ^
                              StringUtil.delimit "," (map tostring outs)^ ")")

            val sys_alloc_pollfds = _import "ml_alloc_pollfds" : int -> int;
            val sys_dealloc_pollfds = _import "free" : int -> unit ;
            val sys_set_pollfds = _import "ml_set_pollfds" : int * int * int * Word16.word -> unit;
            val sys_get_pollfds = _import "ml_get_pollfds" : int * int -> Word16.word;
            val sys_poll = _import "ml_poll" : int * int * int -> int;

            val rd   = _import "ml_poll_rd" : Word16.word ;
            val wr   = _import "ml_poll_wr" : Word16.word ;
            val ex   = _import "ml_poll_ex" : Word16.word ;
            val hup  = _import "ml_poll_hup" : Word16.word ;
            val nval = _import "ml_poll_nval" : Word16.word ;

            infix || &&
            val op && = Word16.andb
            val op || = Word16.orb

            fun notzero 0 = raise RawNetwork "out of memory"
              | notzero x = x

            (* *)
            fun add f ((s, sty), m) =
                case M.find (m, s)
                  (* assumes style hasn't changed!! *)
                 of SOME (f', _) => M.insert (m, s, (f' || f, sty))
                  | NONE => M.insert (m, s, (f, sty))

            (* insert all descriptors in map,
               poll for read and write,
               since exceptions are reported automatically *)
            val fdmap = M.empty
            val fdmap = List.foldl (add rd) fdmap ins
            val fdmap = List.foldl (add wr) fdmap outs
            (* We're always interested in exceptions on
               ins and outs. *)
            val fdmap = List.foldl (add ex) fdmap ins
            val fdmap = List.foldl (add ex) fdmap outs
            val n = M.numItems fdmap

            (* insert all descriptors in array *)
            val fds = notzero (sys_alloc_pollfds n)
            fun ins (s,(f, _),n) = (sys_set_pollfds (fds, n, s, f); n+1)
            val _ = M.foldli ins 0 fdmap

            (* poll *)
            (* on linux 2.4, the timeout argument is milliseconds.
               but I seem to recall switching between msec and usec.
               is it different on other platforms? 
               
                   - Tom7    26 Jun 2007
               *)
            val t = Option.getOpt (Option.map (IntInf.toInt o Time.toMilliseconds) to, ~1)
            val rv = sys_poll (fds, n, t)

            (* create list of sockets with events *)
            fun upd (s, (_, sty), (n, rl, wl, el)) =
                let
                    (* add socket to list if flags intersect *)
                    fun add f f' l =
                        if (f && f') <> 0w0
                        then (s, sty) :: l
                        else l
                    val f' = sys_get_pollfds (fds, n)
                in
                    (n+1,
                     add f' rd rl,
                     add f' wr wl,
                     add f' (ex || hup || nval) el)
                end
            val (_, rds, wrs, exs) = M.foldli upd (0, [], [], []) fdmap
        in
            sys_dealloc_pollfds fds;
            case rv of
                ~1 => raise RawNetwork "poll failed"
              | _ => (rds,wrs,exs)
        end

    fun opensocks () = !numsocks

end
