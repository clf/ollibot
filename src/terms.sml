functor Terms (Rep : TERM_REP) = struct

  open Rep

  type rule_id = int
  type type_id = int

  datatype perm = PERSISTANT | LINEAR 

  datatype prog = 
      P of {intern_table: table}

  datatype fact = 
      F of {perm : perm,
            valid : bool ref,
            echild : efact list ref,
            schild : sfact list ref,
            atom : Lambda.cid * term list}

  and efact = 
      E of {perm : perm,
            valid : bool ref,
            parents : fact list,
            rule : rule_id,
            num : int,
            vars : term list,
            match : term list}

  and sfact = 
      S of {perm : perm,
            valid : bool ref,
            parent : fact,
            rule : rule_id,
            num : int,
            vars : term list,
            match : term list}

  structure Map = 
  RedBlackMapFn(struct type ord_key = int val compare = Int.compare end)

  datatype tree = 
      Node of {fallthrough : tree option,
               seekthrough : tree Map.map option}
    | Leaf of {cstr : int * int list,
               rule : rule_id,
               num : int,
               vars : int list,
               match : int list}

  exception Invariant
  fun match_dtree (fact as F{schild,perm,valid,...}, 
                   prog as P{intern_table,...})
   fn (work, tree, matches) =>
      let 
        fun match (work, tree, matches) = match_dtree (fact, prog)
        fun match_fallthrough(tm, tms, tree) =
            match_dtree (fact, tms, tree, tm :: matches)
        fun match_seekthrough(tm, tms, map) = 
            let val (const_id, spine) = force_const(tm in
              raise Match
            end
      in
        case (work, tree) of
          ([], Leaf{cstr, rule, num, vars, match}) =>             
          let
            val matches = vector matches
            val vars = List.map (fn x => Vector.sub(matches,x)) vars
            val match = List.map (fn x => Vector.sub(matches,x)) match
            val srule = 
                S{perm = perm, valid = valid, parent = fact, 
                  rule = rule, num = num, vars = vars, match = match}
          in
            schild := srule :: !schild;
            [srule]
          end
        | (tm :: tms, Node{fallthrough = NONE, seekthrough = NONE}) => []
        | (tm :: tms, Node{fallthrough = SOME ft, seekthrough = NONE}) => 
          match_dtree (fact, tms, ft, tm :: matches)
        | (tm :: tms, Node{fallthrough = NONE, seekthrough = SOME st}) => 
          Map.foldl 
              (fn (tree, srules) =>
                  match_dtree(fact, tms, tree, matches) @ srules)
              [] st 
        | (tm :: tms, Node{fallthrough = SOME ft, seekthrough = SOME st}) => 
          match_dtree (fact, tms, ft, tm :: matches) @
          Map.foldl 
              (fn (tree, srules) =>
                  match_dtree(fact, tms, tree, matches) :: srules)
              [] st 
        | _ => raise Invariant
      end                     

end
