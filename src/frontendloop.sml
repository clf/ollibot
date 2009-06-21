structure Do = struct

  fun run arg = 
      let in
        print ("=== " ^ arg ^ " === \n\n");
        ignore(Frontend.read arg);
        print ("\nPress enter to continue");
        ignore(TextIO.inputLine TextIO.stdIn)
      end 

  val _ = app run (CommandLine.arguments())

end
