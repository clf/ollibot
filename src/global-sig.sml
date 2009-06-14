(* Ollibot — Robert J. Simmons and Frank Pfenning
 * Signature for the Global structure *)

signature GLOBAL = sig

  exception Error of Pos.pos option * string
  val Err : string -> exn
  val ErrPos : Pos.pos * string -> exn    
                                   
  datatype status = 
	   OK                         (* Normal execution                  *)
	 | ABORT                      (* Program error                     *)
           
  val versionstring : string
                      
  val print : string -> unit
  val liveprint : string -> unit
                        
end

