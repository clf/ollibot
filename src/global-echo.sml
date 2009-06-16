(* Ollibot — Robert J. Simmons and Frank Pfenning
 * Default implementation of the Global structure *)

structure Global :> GLOBAL = struct

  exception Error of Pos.pos option * string 
  val Err = fn s => Error(NONE,s)
  val ErrPos = fn (p,s) => Error(SOME p,s)
                           
  datatype status = 
	   OK 
	 | ABORT  
	   
  val versionstring = "OLLIBOT_VERSION"
                      
  val liveprint =
   fn ">" => print "&gt;"
    | "<" => print "&lt;"
    | "₀" => print "<sub>0</sub>"
    | "₁" => print "<sub>1</sub>"
    | "₂" => print "<sub>2</sub>"
    | "₃" => print "<sub>3</sub>"
    | "₄" => print "<sub>4</sub>"
    | "₅" => print "<sub>5</sub>"
    | "₆" => print "<sub>6</sub>"
    | "₇" => print "<sub>7</sub>"
    | "₈" => print "<sub>8</sub>"
    | "₉" => print "<sub>9</sub>"
    | x => print x
           
  val print = 
   fn "₀" => print "<sub>0</sub>"
    | "₁" => print "<sub>1</sub>"
    | "₂" => print "<sub>2</sub>"
    | "₃" => print "<sub>3</sub>"
    | "₄" => print "<sub>4</sub>"
    | "₅" => print "<sub>5</sub>"
    | "₆" => print "<sub>6</sub>"
    | "₇" => print "<sub>7</sub>"
    | "₈" => print "<sub>8</sub>"
    | "₉" => print "<sub>9</sub>"
    | x => print x
              	  
end

structure MapS = 
RedBlackMapFn(struct type ord_key = string val compare = String.compare end)
