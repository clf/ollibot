(* Implementation of default web interface *)

functor Web(WebHandler : WEB_HANDLER) :> WEB = struct
 
  open WebHandler
  structure R = RawNetwork
  exception Web of string

  fun go NONE = go (SOME DEFAULT_PORT)
    | go (SOME PORT) =
    let
      val server = R.listen PORT

      (* Receives an entire http request over a socket... *)
      fun recvhttp p =
        let
          (* string preceding content length int *)
          val cls = "Content-Length: "
          val size_cls = size cls
                    
          (* Got the header, know how long the content-lenghth is, so
           * receive content until we get that much... *)
          fun morecontent (hs: string) (cl: int) (cs: string) =
            case R.recv p of
              "" => (* XXX what do we assume if connection closed? *)
              (print "closed!\n"; (hs,cs))
            | s' =>
              let val cs = cs ^ s'
              in 
                if size cs >= cl 
                then (print "done\n"; (hs,cs)) (* Normal finish *)
                else morecontent hs cl cs
              end

          (* Getting the header, calling morecontent once we have it... *)
          fun moreheader s = 
            case R.recv p of
              "" => (* XXX what do we assume if connection closed? *)
              (print "closed!\n"; (s, ""))
            | s' => 
              let val s = s ^ s'
              in case StringUtil.find "\r\n\r\n" s of
                NONE => moreheader s
              | SOME n => 
                let
                  val h = n+4 
                  val hs = String.substring(s,0,h)
                  val cs = String.substring(s,h,size s-h)
                in case StringUtil.find cls hs of 
                  NONE => (* Assume no content; should I throw away cs? *)
                  (print "done\n"; (hs,cs))                
                | SOME m => 
                  let
                    val m = m + size_cls
                    val cl = Int.fromString(String.substring(hs,m,size hs-m))
                  in
                    case cl of 
                      NONE => raise Web "Bad content-string"
                    | SOME cl =>
                      (print "content.";
                       morecontent 
                           (String.substring(s,0,h))
                           (cl-h)
                           (String.substring(s,h,size s-h)))
                  end
                end
              end
        in 
          moreheader ("")
        end

      fun sendall p s =
        let
          val x = R.send p s
        in
          if x < 0
          then raise R.RawNetwork "sendall to closed" (* closed? *)
          else
            if x < size s
            then sendall p (String.substring(s, x, size s - x))
            else () 
        end

      fun handle_request (p, addr) =
        let
          val (hdrs, content) = recvhttp p
          val (method, hdrs) = StringUtil.token (StringUtil.ischar #" ") hdrs
          val (url, hdrs) = StringUtil.token (StringUtil.ischar #" ") hdrs
          val nows = 
              Date.fmt 
                  "%a, %d %b %Y %H:%M:%S %Z" (Date.fromTimeLocal (Time.now()))

          fun http (status_code,content_type) = 
              let 
                val code = 
                    case status_code of
                      OK_200 => "200 OK"
                    | NOTFOUND_404 => "404 Not Found"
                    | ERROR_500 => "500 Internal Server Error"
              in 
                "HTTP/1.1 " ^ code ^ "\r\n" ^
                "Date: " ^ nows ^ "\r\n" ^
                "Server: Olliserver\r\n" ^
                "Connection: close\r\n" ^
                "Content-Type: " ^ content_type ^ "\r\n" ^
                "\r\n" 
              end

          (* XXX Actually build headers! XXX *)
          val req_info = 
              {method = case method of 
                          "GET" => GET
                        | "POST" => POST
                        | _ => raise Web("unknown method " ^ method), 
               url = url,
               headers = [], 
               lookup = fn x => NONE}

          val x : (string -> unit) option ref = ref NONE
          val req_fun = 
           fn response =>
              case !x of 
                NONE =>
                let
                  val print = fn s => sendall p s 
                in sendall p (http response); x := SOME print; print end
              | SOME f => f
                 
          val (p1,p2,p3,p4) = R.addressip addr
          val peers = String.concatWith "." (map Int.toString [p1,p2,p3,p4])

        in
          print (peers ^ " - " ^ method ^ " - " ^ url ^ "\n");
          build_reply req_info req_fun;
          (R.hangup p) handle _ => ()
        end
                
      fun loop () =
        let
          val () = print "waiting..."
          val (peer, peera) = R.accept server
        in
          handle_request (peer, peera);
          loop ()
        end

    in
      loop ()
    end

end
