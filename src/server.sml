(* Implementation of the nicities of a top-level loop; hopefully
 * modular in a decent way. Based on the Twelf implementation. 
 *
 * Requires an implementation of RUNLINE. See server-triv.sml for a 
 * trivial example implementation.
 *
 * Rob Simmons *)

signature SERVER = 
sig
    
    (* The server takes  *)
    val server : string * string list -> OS.Process.status
    val go : unit -> OS.Process.status

end (* signature SERVER *)



signature RUNLINE = 
sig

    exception Error of string

    datatype status = 
	     OK                         (* Normal execution                  *)
	   | ABORT                      (* Program error                     *)
	   | NOTHING                    (* Not ready for a response          *)
	   | EXIT of OS.Process.status  (* Terminate the program w/ status   *)

    (* Invoked in the start to deal with command line arguments. 
     * Should only throw the exception Error(str), which will be caught and
     * will cause the program to return with an error. *)
    val start : string * string list -> unit
 
    val runline : string -> status

end (* signature RUNLINE *)



(* Interrupt-catching environment *)

signature SIGINT =
sig

  val interruptLoop : (unit -> unit) -> unit

end (* signature SIGINT *)



functor Server 
	(structure Runline : RUNLINE
	 structure SigINT : SIGINT)
	:> SERVER = struct

  exception Done of OS.Process.status

  fun issue status = 
      case status of 
	  Runline.OK => print("%% OK %%\n")
	| Runline.ABORT => print("%% ABORT %%\n")
	| Runline.NOTHING => ()
	| Runline.EXIT(status) => (print("Goodbye!\n"); raise Done(status))

  fun serveLine() = 
      case TextIO.inputLine (TextIO.stdIn) of
	  NONE => Runline.EXIT(OS.Process.success)
	| SOME s => Runline.runline(s)

  fun serve() = (issue(serveLine()); serve())

  fun serveTop() = 
      serve()
      handle (Runline.Error(str)) => (print("Error: "^str^"\n"); 
				      issue(Runline.ABORT);
				      serveTop())
	   | Done(status) => raise Done(status)
(*	   | exn => (print("Uncaught exception: " ^ exnMessage exn ^ "\n");
		     issue(Runline.ABORT); serveTop())  *)
 
  fun server(name,args) = 
      (Runline.start(name,args);
       SigINT.interruptLoop (fn () => (issue(Runline.OK); serveTop ()));
       OS.Process.failure (* Should never be reached... *))
      handle Runline.Error(_) => OS.Process.failure
	   | Done(status) => status

  fun go() = server("",[])
	
end (* functor Server *)

