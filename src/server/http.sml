(* XXX this should be a functor so we can decide how to chunk up the
   incoming contents. For now we just parse the whole thing as one *)

(* PERF this is written for simplicity, not speed. All the concatenation
   and repeated searching for the header terminator is a real waste. *)

structure Http :> HTTP_PACKETIZER (* where type packet = HttpPacket.http_packet *) =
struct

  exception Http of string

  datatype http_packet =
    Headers of string list
  | Content of string

  type packet = http_packet

  (* we either have some partial headers, or we've seen all the headers,
     including a content-length field that tells us how much data is
     coming, and now we're working on the contents *)
  datatype partialpacket =
    Phead of string
  | Pcont of int * string
  | Pdone

  val empty = Phead ""

  fun parse (Phead s) new =
    let val a = s ^ new
    in
      case StringUtil.find "\r\n\r\n" a of
        (* headers not done yet.. *)
        NONE => (nil, Phead a)
      | SOME i =>
          let val hchunk = String.substring(a, 0, i)
              val hchunk = StringUtil.filter (fn #"\r" => false | _ => true) hchunk
              val headers = String.tokens (fn #"\n" => true | _ => false) hchunk

              val cl =
                case List.find (StringUtil.matchhead "Content-Length:") headers of
                  NONE => 
                    let in
                      (*
                      app (fn s => print ("  "^ s ^ "\n")) headers;
                      raise Http "no content-length header"
                      *)
                      (* assume empty contents then... *)
                      0
                    end
                | SOME h => 
                    case 
                      Int.fromString(List.last
                                     (String.tokens
                                      (fn c => c = #" " orelse
                                       c = #"\t") h)) of
                      NONE => raise Http "content-length wasn't an integer?"
                    | SOME x => x


              val j = i + 4 (* size of \r\n\r\n *)
          in
            continue ([Headers headers], Pcont (cl, String.substring(a, j, size a - j)))
          end
      end
    | parse (Pcont (len, s)) new =
      let
        val a = s ^ new
      in
        if size a >= len
        then continue ([Content (String.substring(a, 0, len))], Pdone)
        else (nil, Pcont (len, a))
      end
    (* we'll never return any more packets... *)
    | parse Pdone _ = (nil, Pdone)

  (* give it another chance to find more packets in the input *)
  and continue (l, pp) =
    let val (ll, ppp) = parse pp ""
    in (l @ ll, ppp)
    end

  (* inserts CRLF at the end of http lines *)
  fun make _ = raise Http "sending unimplemented" (* would be easy, just don't need it now *)

  val protocol = Network.protocol { make = make,
                                    parse = parse,
                                    empty = empty,
                                    desc = "HTTP" }

end
