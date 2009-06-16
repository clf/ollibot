
fun message e =
  (case e of
     Loop.Loop s => "loop: " ^ s
   | RawNetwork.RawNetwork s => "rn: " ^ s
   | Network.Network s => "network: " ^ s
   | Session.Session s => "session: " ^ s
   | Session.Expired => "session expired and not caught!"
   | Execute.Execute s => "execute: " ^ s
   | Http.Http s => "http: " ^ s
   | Marshal.Marshal s => "marshal: " ^ s
   | _ => exnMessage e)

val () = 
    case Params.docommandline () of
        nil => 
          (let in
             TrivialDB.init ();
             Loop.init ();
             Loop.loop ()
           end handle e => print ("ERROR: " ^ message e ^ "\n")
             )
      | _ =>
          let in
            print "\nThis program takes no arguments.\n\n";
            print (Params.usage ())
          end
