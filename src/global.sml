(* Ollibot — Robert J. Simmons and Frank Pfenning
 * Default implementation of the Global structure *)

structure Global :> GLOBAL = struct

  exception Error of Pos.pos option * string 
  val Err = fn s => Error(NONE,s)
  val ErrPos = fn (p,s) => Error(SOME p,s)
                           
  datatype status = 
	   OK 
	 | ABORT  
	   
  val versionstring = "0.1"
                      
  val print = print
  val liveprint = ignore
              	  
end

structure MapS = 
RedBlackMapFn(struct type ord_key = string val compare = String.compare end)
