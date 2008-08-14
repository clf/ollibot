functor CommentFn(X : COMMENT_DEFINITION) :> COMMENT where type char = X.char =
struct

type char = X.char
type str = X.char list
type stream = (char * Pos.pos) Stream.stream
type front = (char * Pos.pos) Stream.front

structure Def = X
open Def
structure MapC = SplayMapFn(struct 
                            type ord_key = char 
                            val compare = Def.compare 
                            end)

fun ==(c1,c2) = case Def.compare(c1,c2) of EQUAL => true | _ => false
fun ===(s1,s2) = List.all == (ListPair.zip(s1,s2))

infix 4 == ===

datatype tree = 
    Step  of tree MapC.map
  | Line  of {opening: str, closing: char}
  | Open  of {opening: str, closing: str, ignore: bool}
  | Close of {opening: str, closing: str, ignore: bool}
  | EOF   of {opening: str}

datatype action = 
    ActionEnd
  | ActionCons  of Pos.pos * stream * char
  | ActionLine  of Pos.pos * stream * char
  | ActionOpen  of Pos.pos * stream * {opening: str, closing: str, ignore: bool}
  | ActionClose of Pos.pos * stream * {opening: str, closing: str, ignore: bool}

exception Ambiguous
fun insert_tree(c :: cs, SOME(Step old_map), d) = 
    let 
      val old_subtree = MapC.find(old_map,c)
      val new_subtree = insert_tree(cs, old_subtree, d) 
      val new_map = MapC.insert(old_map, c, new_subtree)
    in Step(new_map) end
  | insert_tree(c :: cs, SOME _, d) = raise Ambiguous
  | insert_tree(c :: cs, NONE, d) = 
    let 
      val new_subtree = insert_tree(cs, NONE, d)
    in Step(MapC.singleton(c,new_subtree)) end
  | insert_tree([], NONE, d) = d
  | insert_tree([], _, d) = raise Ambiguous

val action_tree = 
    let 
      fun ins_o (data,tree) = insert_tree(#opening data,SOME(tree),Open(data))
      fun ins_c (data,tree) = insert_tree(#closing data,SOME(tree),Close(data))
      fun ins_e (data,tree) = insert_tree(#opening data,SOME(tree),EOF(data))
      fun ins_l (data,tree) = insert_tree(#opening data,SOME(tree),Line(data))
      val tree = List.foldr ins_o (Step MapC.empty)  Def.bracketed
      val tree = List.foldr ins_c tree Def.bracketed
      val tree = List.foldr ins_e tree Def.eof
      val tree = List.foldr ins_l tree Def.line
    in case tree of 
         Step map => map
       | _ => raise Domain (* Impossible *)
    end

(* analyze takes a stream and determines the next action on that stream *)
val analyze = 
 fn str =>
    let 
      (* Just returns a char or end-of-file, nothing interesting *)
      val default = Stream.force str
      val step = 
          case Stream.force str of 
            Stream.Nil => ActionEnd
          | Stream.Cons((c,pos),str') => (ActionCons(pos,str',c))

      (* Tries to find a special case and runs step() if it fails *)
      fun seek str pos map =
          case Stream.force str of 
            Stream.Nil => step
          | Stream.Cons((c,pos'),str') =>
            let val newpos = Pos.union(pos,pos') in
              case MapC.find(map,c) of 
                NONE => (step)
              | SOME(Step(map'))  => (seek str' newpos map')
              | SOME(Line(data))  => (ActionLine(newpos, str', #closing data))
              | SOME(Open(data))  => (ActionOpen(newpos, str', data))
              | SOME(Close(data)) => (ActionClose(newpos, str', data))
              | SOME(EOF _)       => (ActionEnd)
            end
    in
      case default of 
        Stream.Nil => ActionEnd
      | Stream.Cons((c,pos),_) => seek str pos action_tree
    end

(* scans forward, presumably for an end-of-line character *)
fun scan_forward str pos chr = 
    case Stream.force str of 
      Stream.Nil => (pos,Stream.empty)
    | Stream.Cons((c,pos'),str') => 
      let val newpos = Pos.union(pos,pos') in
        if c == chr 
        then (newpos,str')
        else scan_forward str' newpos chr
      end


val filter =
    let
      fun advance str : stream = 
          case Stream.force str of 
            Stream.Nil => Stream.empty 
          | Stream.Cons(_,str) => str

      (* Ensure that there are no unclosed comments and end stream *)
      fun end_handler cs : front = 
          case cs of 
            [] => Stream.Nil 
          | (pos,_) :: _ => (Def.unclosed pos; Stream.Nil)

      fun filter str cs : stream = Stream.delay(step str cs)          

      (* Call Def.comments and correctly generate altered output *)
      and comment_handler (pos,str) cs : front = 
          (Def.comments pos; 
           case Def.replace of 
             NONE => step str cs ()
           | SOME c => Stream.Cons((c,pos), filter str cs))

      and step_comment str cs pos (delim: Pos.pos * str) : Pos.pos * stream = 
          case analyze str of 
            ActionEnd => (Def.unclosed (#1 delim); (pos,Stream.empty))

          | ActionCons(pos',str',_) => 
            step_comment str' cs (Pos.union(pos,pos')) delim

          | ActionLine(pos',str',c) => 
            let val (pos'',str'') = scan_forward str' pos' c 
            in step_comment str'' cs (Pos.union(pos,pos'')) delim end

          | ActionOpen(pos',str',{closing,...}) =>
            step_comment str' ((pos,closing) :: cs) (Pos.union(pos,pos')) delim
          
          | ActionClose(pos',str',{closing,...}) =>
            if not(null cs) andalso #2(hd cs) === closing
            then step_comment str' (tl cs) (Pos.union(pos,pos')) delim
            else if null cs andalso closing === #2(delim)
            then (Pos.union(pos,pos'), str')
            else (Def.unmatched pos; step_comment (advance str) cs pos delim)

      and step str cs () : front = 
          case analyze str of 
            ActionEnd => end_handler cs

          | ActionCons(pos,str',c) => 
            Stream.Cons((c,pos),filter str' cs)

          | ActionLine(pos,str',c) => 
            comment_handler (scan_forward str' pos c) cs

          | ActionOpen(pos,str',{closing,ignore=true,...}) =>
            comment_handler (step_comment str' [] pos (pos,closing)) cs

          | ActionOpen(pos,str',{closing,ignore=false,...}) =>
            comment_handler (pos,str') ((pos,closing) :: cs)

          | ActionClose(pos,str',{closing,...}) =>
            if not(null cs) andalso #2(hd cs) === closing
            then comment_handler (pos, str') (tl cs)
            else (Def.unmatched pos; step (advance str) cs ())
    in fn str => filter str [] end

end
