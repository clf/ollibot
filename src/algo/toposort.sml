functor TopoSort (Key : ORD_KEY where type ord_key = string) :> TOPOSORT where type member = Key.ord_key =
struct

    structure Map = RedBlackMapFn(Key)

    type member = Key.ord_key
    datatype constraint = 
         HARD of member * member
       | SOFT of member * member    
    type result = int Map.map

    exception TopoSort of member * member

    val constraint_lt = HARD
    val constraint_leq = SOFT

    val strip = map (fn HARD x => x | SOFT x => x)

    fun vert_insert (v, edges) =
        if Map.inDomain (edges, v) then edges else Map.insert (edges, v, [])

    fun graph_insert ((v1,v2),(edges,redges)) =
        let
          val edges = vert_insert(v1,vert_insert(v2, edges))
          val redges = vert_insert(v1,vert_insert(v2, redges))
        in (Map.insert(edges, v1, v2 :: Map.lookup(edges, v1)),
            Map.insert(redges, v2, v1 :: Map.lookup(redges, v2))) end

    fun make_graph cstrs = foldr graph_insert (Map.empty, Map.empty) cstrs

    fun dfs_visit edges =
        let
          fun dfs (v : member, (unseen, stack)) = 
              if not (Map.inDomain(unseen, v)) then (unseen, stack)
              else let val (unseen, succ) = Map.remove (unseen,v)
                val (unseen, stack) = foldr dfs (unseen,stack) succ
              in (unseen, v :: stack) end
          fun loop (unseen, stack) = 
              case Map.firsti unseen of
                NONE => stack
              | SOME (v,_) => loop (dfs (v, (unseen, stack)))
        in loop (edges, []) end

    fun dfs_revisit (redges, stack) = 
        let
          fun dfs (v : member, (n, nums)) = 
              if Map.inDomain (nums, v) then (n, nums)
              else let val nums = Map.insert(nums, v, n)
              in (* print ("Revisiting " ^ v ^ "\n"); *)
                 foldr dfs (n, nums) (Map.lookup (redges, v)) end
          fun loop ([], nums) = nums
            | loop (v :: stack, nums) = 
              (* print ("Looping at " ^ v ^ "\n"); *)
              if Map.inDomain (nums, v) then loop (stack, nums)
              else loop(stack, #2(dfs(v, (Map.numItems nums, nums))))
        in loop (stack, Map.empty) end

    val get_all = Map.listItemsi
    val get = Map.lookup

    exception Invariant
    fun sort cstrs = 
        let
          fun dc (_, []) = ()
            | dc (map, HARD(a,b) :: cstrs) = 
              if get (map, a) = get (map, b)
              then raise TopoSort(a,b)
              else if get (map, a) > get (map, b)
              then raise Invariant
              else dc (map, cstrs)
            | dc (map, SOFT(a,b) :: cstrs) = 
              if get (map, a) > get (map, b)
              then raise Invariant 
              else dc (map, cstrs)
          fun doublecheck map = dc (map, cstrs)
          val cstrs = strip cstrs
          val (edges, redges) = make_graph cstrs
          val stack = dfs_visit edges
          val nums = dfs_revisit (redges, stack)
          (* val () = Map.appi (fn (s,i) => 
             print ("Key: " ^ s ^ " Value: " ^ Int.toString i ^ "\n")) nums *)
        in doublecheck nums; nums end

end
