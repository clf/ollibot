(* Signatures for web interface *)

signature WEB_HANDLER = sig

  (* Default port number, unless go is given a different port number *)
  val DEFAULT_PORT : int

  (* Limited subset of status codes are provided *)
  datatype request_method = GET | POST

  (* Limited subset of status codes are provided *)
  datatype status_code = OK_200 | NOTFOUND_404 | ERROR_500
  type content_type = string

  type request_info = 
    {
     method: request_method,                 
     url: string,                    (* Request URL *)
     headers: string list,           (* A list of all headers with values *)
     lookup: string -> string option (* Lookup the value of a header *)
    }     

  (* build_reply is the user's (imperative) implementation of building HTTP
   * responses.
   * 
   * The framework will provide the request information object and a function;
   * this function, upon being given a status code, returns a "print" function
   * that prints to the output stream. *)
  val build_reply : 
      request_info -> (status_code * content_type -> string -> unit) -> unit

end

signature WEB = sig

  (* Takes an optional port number and starts the webserver *)
  val go : int option -> 'a

end

