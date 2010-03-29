signature TOPOSORT =
sig

    type member
    type constraint
    type result
    
    exception TopoSort of member * member

    (* constraint_lt a b => "a must appear before b" *)
    val constraint_lt : member * member -> constraint

    (* constraint_leq a b => "a must appear no later than b" *)
    val constraint_leq : member * member -> constraint

    val sort : constraint list -> result

    val get_all : result -> (member * int) list
    val get : result * member -> int

end
