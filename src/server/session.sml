(* A session is a running instance of a program along with
   the connections with the client and a unique integer that
   identifies it.

   XXX in failure situations, we shouldn't shut down the
   entire server...
   
   *)

structure Session :> SESSION =
struct

  val port = Params.param "5555"
        (SOME("-port",
              "The port to listen on."))
        "port"

  val host = Params.param "gs82.sp.cs.cmu.edu"
        (SOME("-host",
              "The host to listen on."))
        "host"

  val codepath = Params.param "../ml5pgh/tests/"
        (SOME("-codepath",
              "Where to find .b5, .js, .ml5 code, etc."))
        "codepath"

  val staticpath = Params.param "../static/"
        (SOME("-staticpath",
              "Where to find GIFs and other static data"))
        "staticpath"

  val favicon_ico = StringUtil.readfile "favicon.ico"
  val logo_png    = StringUtil.readfile "../graphics/server5-logo-striped.png"
  val faviconhead = "<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />"

  infixr 9 `
  fun a ` b = a b

  structure N = Network

  datatype toclient =
    Open of N.sock
    (* the last time we saw the socket open *)
  | Closed of Time.time

  datatype toserver =
    Waiting of N.sock
  | Data of string

  local
    val ctr = ref 0
  in
    fun newid () =
      let in
        ctr := !ctr + 1;
        !ctr
      end
  end

  datatype session = S of { id : int,
                            (* sockets the client is using to send us messages *)
                            toserver : toserver list ref,
                            (* socket the client keeps open for us to send
                               messages on *)
                            toclient : toclient ref,

                            (* the running instance, which keeps the thread
                               state, etc. *)
                            inst : Execute.instance 
                            
                            }

  (* A session is no longer valid *)
  exception Expired
  exception Session of string

  val sessions = ref (nil : session list)

  fun id (S { id, ... }) = id
  fun getsession i = ListUtil.example (fn (S { id, ...}) => id = i) ` !sessions

  fun opensockets (S { toserver, toclient, ... }) =
    (List.concat ` map (fn Waiting s => [s] | _ => nil) ` !toserver) @
    (case !toclient of Open s => [s] | Closed _ => nil)

  fun getbysock sock =
      ListUtil.extract
      (fn session => List.exists (fn s' => N.eq (sock, s')) ` 
       opensockets session) ` !sessions

  (* Any toserver/toclient sockets that we have open *)
  fun sockets () =
    List.concat ` 
      map opensockets ` !sessions

  (* doesn't remove it from the list, just removes any resources *)
  fun destroy (session as S { inst, id, ... }) =
    let in
      print ("I am destroying session #" ^ Int.toString id ^ ".\n");
      app (fn s => N.disconnect s handle _ => ()) ` opensockets session;
      Execute.destroy inst
    end

  fun closed sock =
    case getbysock sock of
      NONE => raise Session "(closed) no such socket active in sessions??"
    | SOME (session as S { toserver, toclient, ... }, rest) =>
        let
          fun fatal () =
            let in
              destroy session;
              sessions := rest
            end
        in
          (* Client is only allowed to close toserver connections. Filter 'em. *)
          toserver := List.filter (fn Data _ => true | Waiting s => not ` N.eq (s, sock)) ` !toserver;

          (* If this closes, game over *)
          (case !toclient of
             Open s' => if N.eq (s', sock) then fatal () else ()
           | Closed _ => ())
        end

  fun packet (Http.Headers _, _) = raise Session "got headers again??"
    | packet (Http.Content c, sock) =
    case getbysock sock of
      NONE => raise Session "(packet) no such socket active in sessions??"
    | SOME (session as S { toserver, toclient, ... }, rest) =>
        let in
          (* maybe we'll see empty content, depending on the browser.
             anyway we don't care about it. *)
          (case !toclient of
             Open s' => if N.eq (s', sock) 
                        then print "content for toclient? ignored!\n"
                        else ()
           | Closed _ => ());
          
          (* should be for a toserver connection. *)
          toserver := map (fn Data d => Data d
                            | Waiting s' => if N.eq(s', sock) 
                                            then (print ("DATA " ^ c ^ "\n");
                                                  N.disconnect s'; 
                                                  Data c)
                                            else Waiting s') ` !toserver

          (* XXX step here to deliver messages? *)
        end
    

  fun failnew s prog str =
    let in
      N.sendraw s
      ("HTTP/1.1 404 URL Not Found\r\n" ^
       "Date: " ^ Version.date () ^ "\r\n" ^
       "Server: " ^ Version.version ^ "\r\n" ^
       "Connection: close\r\n" ^
       "Content-Type: text/html; charset=utf-8\r\n" ^
       "\r\n" ^
       "'<b>" ^ prog ^ "</b>' was not found: " ^ str ^ "\n");
      N.disconnect s
    end

  fun new s prog =
    let
      val id = newid ()

      (* don't allow the user to read files outside the codepath. *)
      val () = case StringUtil.find ".." prog of NONE => () | SOME _ => raise Session "illegal path"
      val () = case StringUtil.find "/"  prog of NONE => () | SOME _ => raise Session "illegal path"

      (* XXX paths should be from a config file *)
      val sbc = BytecodeParse.parsefile (!codepath ^ prog ^ "_server.b5")
      val rt = StringUtil.readfile "../ml5pgh/js/runtime.js"
      val js = StringUtil.readfile (!codepath ^ prog ^ "_home.js")

      val sessiondata =
        (* XXX should be from a config file *)
        "var session_serverurl = 'http://" ^ !host ^ ":" ^ !port ^ "/toserver/';\n" ^
        "var session_clienturl = 'http://" ^ !host ^ ":" ^ !port ^ "/toclient/';\n" ^
        "var session_id = " ^ Int.toString id ^ ";\n"

      (* this could be better... ;) 
         probably it should act like a javascript <link> so that we
         can write the html elsewhere? *)
      val data =
        "<html><head>\n" ^
        "<title>Server 5 Test Page!</title>\n" ^
        "<script language=\"JavaScript\">\n" ^ sessiondata ^ "</script>\n" ^
        "<script language=\"JavaScript\">\n" ^ rt ^ "\n</script>\n" ^
        "<script language=\"JavaScript\">\n" ^ js ^ "\n</script>\n" ^
        faviconhead ^
        CSS.csshead ^ 
        "</head>\n" ^
        "<body>\n" ^
        "<p><div id=\"page\"></div>\n" ^
        "<div id=\"messages\"><hr /><br />Server 5 debugging stuff:\n" ^
        "<p></div>\n" ^ 
        "</body></html>\n";

      val res = ("Content-Type: text/html; charset=utf-8\r\n" ^
                 "\r\n" ^
                 data)

    in
      N.sendraw s
      ("HTTP/1.1 200 OK\r\n" ^
       "Date: " ^ Version.date () ^ "\r\n" ^
       "Server: " ^ Version.version ^ "\r\n" ^
       "Connection: close\r\n" ^
       "X-Program: " ^ prog ^ "\r\n" ^
       "X-Sessionid: " ^ Int.toString id ^ "\r\n" ^
       "Content-Type: text/html; charset=utf-8\r\n" ^
       "\r\n" ^
       data);
      N.disconnect s;
      sessions := S { id = id, toserver = ref nil, 
                      toclient = ref ` Closed ` Time.now (),
                      inst = Execute.new sbc } :: !sessions
    end handle BytecodeParse.BytecodeParse msg => failnew s prog ("parse error: " ^ msg)
             | IO.Io { function, name, ... } => failnew s prog ("IO error: " ^ function ^ " / " ^ name)
             | Session str => failnew s prog ("Session error: " ^ str)

  (* make progress on any instance where we can *)
  fun step () =
    let
      val progress = ref false
      fun onesession (S { inst, toserver, toclient, ... }) =
        let
          (* process incoming requests in order. *)
          fun incoming nil = nil
            | incoming (Data d :: rest) = (Execute.come inst d;
                                           progress := true;
                                           incoming rest)
            | incoming l = l

          fun outgoing (ref (Closed _)) = () (* XXX should expire old sessions here *)
            | outgoing (r as ref (Open sock)) =
            case Execute.message inst of
              NONE => ()
            | SOME m =>
                let 
                  val m = "<lambda5message>" ^ m ^ "</lambda5message>"
                in
                  N.sendraw sock m;
                  N.disconnect sock;
                  (* one message per connection *)
                  progress := true;
                  r := (Closed ` Time.now ())
                end
        in
          (* incoming requests. *)
          toserver := incoming ` !toserver;

          (* an outgoing message (at most one) *)
          outgoing toclient;

          (* run some waiting thread *)
          progress := (Execute.step inst orelse !progress)
        end
    in
      (* print "step..\n"; *)
      List.app onesession ` !sessions;
      !progress
    end

  fun toserver sock id =
    case getsession id of
       SOME (S { toserver, ... }) => toserver := !toserver @ [Waiting sock]
     | NONE => raise Expired

  fun toclient sock id = 
    case getsession id of
      (* what to do if it's already open?? should be fatal? *)
      SOME (S { toclient, ...}) =>
        (case !toclient of
           Open _ => raise Session "client made a second toclient socket"
         | Closed _ => toclient := Open sock)
    | NONE => raise Expired

  fun demos sock =
    let
      val l = StringUtil.readfile (!codepath ^ "demos.txt")
      val l = String.tokens Char.isSpace l
      fun pr str = N.sendraw sock str

      fun link name ext = "<td><a href=\"/source/" ^ name ^ "." ^ ext ^ "\">" ^ name ^ "." ^ ext ^ "</a></td>"
      val even = ref true
      fun onedemo basename =
        let in
          pr ("<tr class=\"row_" ^ (if !even then "even" else "odd") ^
              "\">" ^ link basename "ml5"
              ^ link (basename ^ "_home") "js"
              ^ link (basename ^ "_server") "b5"
              ^ "<td><a class=\"rundemo\" href=\"/5/" ^ basename ^ "\">run " ^ basename ^ " now</a></td>"
              ^ "</tr>\n");
          even := not (!even)
        end
    in
      pr
      ("HTTP/1.1 200 OK\r\n" ^
       "Date: " ^ Version.date () ^ "\r\n" ^
       "Server: " ^ Version.version ^ "\r\n" ^
       "Connection: close\r\n" ^
       "Content-Type: text/html; charset=utf-8\r\n" ^
       "\r\n");

      pr "<html><head><title>Server 5 demos</title></head>";
      pr faviconhead;
      pr CSS.csshead;
      pr "<body>\n";
      pr "<img src=\"/static/logo.png\" style=\"float:left;\" />Server 5 demos list.\n";
      pr "<table width=\"100%\">\n";
      pr "<tr><td>ML5 source code</td><td colspan=2>Compiled code</td><td>Demo</td></tr>\n";
      app onedemo l;
      pr "</table>\n";
      pr "</body></html>\n";

      N.disconnect sock
    end

  fun escape s =
    StringUtil.replace "<" "&lt;"
    (StringUtil.replace ">" "&gt;"
     (StringUtil.replace "&" "&amp;" s))

  (* XXX maybe more checks to see if this is a source file?
     Right now they can read anything in the codepath directory. *)
  fun source sock file =
    if CharVector.exists (StringUtil.charspec "^-A-Za-z0-9._") file
    then failnew sock file "it contains illegal characters"
    else
      let
        fun pr str = N.sendraw sock str
        val contents = escape (StringUtil.readfile (!codepath ^ file))
      in
        pr
        ("HTTP/1.1 200 OK\r\n" ^
         "Date: " ^ Version.date () ^ "\r\n" ^
         "Server: " ^ Version.version ^ "\r\n" ^
         "Connection: close\r\n" ^
         "Content-Type: text/html; charset=utf-8\r\n" ^
         "\r\n");

        pr "<html><head>";
        pr faviconhead;
        pr CSS.csshead;
        pr "</head><body>";
        pr "<pre>\n";
        pr contents;
        pr "</pre>\n";

        N.disconnect sock
      end handle Io => failnew sock file "the file was not found"

  fun type_from_filename f =
    case FSUtil.splitext (StringUtil.lcase f) of
      (_, ".gif") => "image/gif"
    | (_, ".jpg") => "image/jpeg"
    | (_, ".png") => "image/png"
    | (_, ".ico") => "image/x-icon"
    | (_, _) => "application/octet-stream"
  (* ; charset=utf-8 ? *)

  fun static sock file =
    if CharVector.exists (StringUtil.charspec "^-A-Za-z0-9._") file
    then failnew sock file "it contains illegal characters"
    else
      let
        fun pr str = N.sendraw sock str
        val contents = StringUtil.readfile (!staticpath ^ file)
      in
        pr
        ("HTTP/1.1 200 OK\r\n" ^
         "Date: " ^ Version.date () ^ "\r\n" ^
         "Server: " ^ Version.version ^ "\r\n" ^
         "Connection: close\r\n" ^
         "Content-Type: " ^ type_from_filename file ^ "\r\n" ^
         "\r\n");

        pr contents;
        N.disconnect sock
      end handle Io => failnew sock file "the file was not found"

   fun favicon sock =
      let
        fun pr str = N.sendraw sock str
      in
        print "server icon\n";
        pr
        ("HTTP/1.1 200 OK\r\n" ^
         "Date: " ^ Version.date () ^ "\r\n" ^
         "Server: " ^ Version.version ^ "\r\n" ^
         "Connection: close\r\n" ^
         "Content-Type: image/x-icon\r\n" ^
         "\r\n");
        pr favicon_ico;
        N.disconnect sock
      end

end
