(* A default web handler to start with *)

structure DefaultHandler = struct
 
  val DEFAULT_PORT = 80
  datatype request_method = GET | POST
  datatype status_code = OK_200 | NOTFOUND_404 | ERROR_500
  type content_type = string

  type request_info = 
    {
     method: request_method,                 
     url: string,          
     headers: string list, 
     lookup: string -> string option
    }     

  fun not_found_page f = 
      let 
        val print : string -> unit = 
            f (NOTFOUND_404, "text/html; charset=utf-8")
      in 
        print "<html>\n";
        print "<head><title>Page Not Found</title><head>\n";
        print "<body>The page you requested was not found.</body>\n";
        print "</http>\n";
        ()
      end

  fun build_reply _ f = not_found_page f

end

