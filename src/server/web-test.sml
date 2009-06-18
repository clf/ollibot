(* Simple web handler that demonstrates the different types of pages *)

structure SimpleHandler :> WEB_HANDLER = struct
 
  open DefaultHandler

  val DEFAULT_PORT = 2999

  fun build_reply {method,url = "/thebadURL",headers,lookup} 
      (f: status_code * content_type -> string -> unit)  = 
      let 
        val print = f (NOTFOUND_404,"text/html; charset=utf-8")
      in
        print "<html>\n";
        print "<head><title>Page Not Found</title></head>\n";
        print "<body>You found the page that isn't found!</body>\n";
        print "</html>\n";
        ()
      end
    | build_reply {method,url = "/thereallybadURL",headers,lookup} f =
      let 
        val print = f (ERROR_500,"text/html; charset=utf-8")
      in
        print "<html>\n";
        print "<head><title>Internal Server Error</title></head>\n";
        print "<body>Ooh, that's really bad</body>\n";
        print "</html>\n";
        ()
      end
    | build_reply {method,url = "/favicon.ico",headers,lookup} f = 
      (let
         val favicon_ico = StringUtil.readfile "favicon.ico"
         val print = f (OK_200,"image/x-icon")
       in
         print favicon_ico
      end
      handle exn => 
             let 
               val print = f (NOTFOUND_404,"text/html; charset=utf-8")
             in
               print "<html>\n";
               print "<head><title>Page Not Found</title></head>\n";
               print "<body>You found the page that isn't found!</body>\n";
               print "</html>\n";
               ()
             end)
    | build_reply {method,url,headers,lookup} f = 
      let
        val print = f (OK_200,"text/html; charset=utf-8")
      in
        print "<html>\n";
        print "<head><title>Regular Page</title></head>\n";
        print "<body>\n";
        print " <h1>You have found a page!</h1>\n";
        print(" <p>The url for this page is " ^ url ^ "</p>\n");
        print "</body>\n";
        print "</html>\n";
        ()
      end

end

structure SimpleWeb = Web(SimpleHandler)
val _ = SimpleWeb.go NONE
