structure Do = struct

  fun run arg = 
      let in
        print ("=== " ^ arg ^ " === \n\n");
        ignore(Frontend.read arg)
      end 

  val _ = app run (CommandLine.arguments())

end
