structure Wiki = struct

  (* Some text utilities *)
  val replace = 
   fn ">" => "&gt;"
    | "<" => "&lt;"
    | "&" => "&amp;"
    | "₀" => "<sub>0</sub>"
    | "₁" => "<sub>1</sub>"
    | "₂" => "<sub>2</sub>"
    | "₃" => "<sub>3</sub>"
    | "₄" => "<sub>4</sub>"
    | "₅" => "<sub>5</sub>"
    | "₆" => "<sub>6</sub>"
    | "₇" => "<sub>7</sub>"
    | "₈" => "<sub>8</sub>"
    | "₉" => "<sub>9</sub>"
    | x => x

  val replace_pre = 
   fn "\n" => "<br />\n"
    | " " => "&nbsp;"
    | ">" => "&gt;"
    | "<" => "&lt;"
    | "&" => "&amp;"
    | "₀" => "<sub>0</sub>"
    | "₁" => "<sub>1</sub>"
    | "₂" => "<sub>2</sub>"
    | "₃" => "<sub>3</sub>"
    | "₄" => "<sub>4</sub>"
    | "₅" => "<sub>5</sub>"
    | "₆" => "<sub>6</sub>"
    | "₇" => "<sub>7</sub>"
    | "₈" => "<sub>8</sub>"
    | "₉" => "<sub>9</sub>"
    | x => x

  val preformat = UTF8Util.translate replace_pre

  val htmlify = map replace

  (* Some list utilities *)
  fun matchhead small big = 
    case (small, big) of 
      ([],_) => true
    | (a :: small,b :: big) => 
      if a = b then matchhead small big else false
    | _ => false

  fun trimleft [] = []
    | trimleft (" " :: cs) = trimleft cs
    | trimleft ("\t" :: cs) = trimleft cs
    | trimleft ("\n" :: cs) = trimleft cs
    | trimleft cs = cs

  fun trim [] = []
    | trim (" " :: cs) = trim cs
    | trim ("\t" :: cs) = trim cs
    | trim ("\n" :: cs) = trim cs
    | trim cs = rev(trimleft(rev cs))
           
  (* Tries to split a string at a separator *)
  fun split small big = 
    let 
      val matches = matchhead small
      fun loop (first, second) = 
         if matches second then SOME (rev first, second)
         else case second of 
                [] => NONE
              | a :: second => loop (a :: first, second)
    in loop ([],big) end

  (* wikify : take a list of UTF8 codepoints and build html *)
  fun wikify cs = 
    let exception Wikify of string 
                            
      fun drop ([],big) = big
        | drop (x :: small, y :: big) = drop(small,big)
        | drop _ = raise Empty
                         
      (* Wikifiy a [[link]] or [[address|link]] *)
      (* XXX Check for existance of local links! *)
      fun process_link cs = 
        let in
          case split ["]","]"] cs of
            NONE => raise Wikify "No matching ]]" 
          | SOME(link,cs) => 
            let
              val (url,text) = 
                case split ["|"] (trim link) of 
                  NONE => (concat link, concat(htmlify link))
                | SOME (url,text) => 
                  (concat(trim url), wikify (trim (tl text)))
            in
              ("<a href=\"" ^ url ^ "\">" ^ text ^ "</a>", tl(tl(cs)))
            end
        end
        
      (* Wikify =some= ===heading=== ======type====== *)
      fun process_heading (term,n) cs = 
        let in
          case split term cs of
            NONE => raise Wikify ("No matching " ^ concat term)
          | SOME(heading,cs) => 
            ("<h" ^ Int.toString n ^ ">" ^ 
             (wikify (trim heading)) ^
             "</h" ^ Int.toString n ^ ">\n",
             drop (term,cs))
        end
        
      fun process cs output = 
        let in
          case cs of
            [] => concat(rev output)

          (* Formatting (bold, italics, preformatted) *)
          | "`" :: cs =>
            let in
              case split ["`"] cs of
                NONE => raise Wikify "No matching `" 
              | SOME(escaped,cs) => 
                let val quoted = concat (map replace_pre escaped)
                in process (tl cs)
                           ("<tt><b>" ^ quoted ^ "</b></tt>" :: output) 
                end
            end
          | "'" :: "'" :: "'" :: cs =>
            let in
              case split ["'", "'", ","] cs of
                NONE => raise Wikify "No matching `" 
              | SOME(bold,cs) => 
                let val bold = wikify bold
                in 
                  process (tl (tl (tl cs)))
                          ("<b>" ^ bold ^ "</b>" :: output)
                end
            end
          | "'" :: "'" :: cs =>
            let in
              case split ["'", "'"] cs of
                NONE => raise Wikify "No matching `" 
              | SOME(italics,cs) => 
                let val italics = wikify italics
                in 
                  process (tl (tl cs)) 
                          ("<i>" ^ italics ^ "</i>" :: output)
                end
            end
            
          (* Links *)
          | "[" :: "[" :: cs =>
            let val (link,cs) = process_link cs 
            in process cs (link :: output) end
            
          (* Headings *)
          | "=" :: "=" :: "=" :: "=" :: "=" :: "=" :: cs =>
            let val (heading,cs) = 
                    process_heading (["=","=","=","=","=","="], 6) cs
            in process cs (heading :: output) end
          | "=" :: "=" :: "=" :: "=" :: "=" :: cs =>
            let val (heading,cs) = 
                    process_heading (["=","=","=","=","="], 5) cs
            in process cs (heading :: output) end
          | "=" :: "=" :: "=" :: "=" :: cs =>
            let val (heading,cs) = process_heading (["=","=","=","="], 4) cs
            in process cs (heading :: output) end
          | "=" :: "=" :: "=" :: cs =>
            let val (heading,cs) = process_heading (["=","=","="], 3) cs
            in process cs (heading :: output) end
          | "=" :: "=" :: cs =>
            let val (heading,cs) = process_heading (["=","="], 2) cs
            in process cs (heading :: output) end
          | "=" :: cs =>
            let val (heading,cs) = process_heading (["="], 1) cs
            in process cs (heading :: output) end
            

          (* Default case: merrily replace *)
          | c :: cs => process cs (replace c :: output) 
                                         
        end handle Wikify(msg) => 
                   concat(rev output) ^ 
                   "<span class=\"error\">Error: " ^ msg ^ "</span>" ^
                   concat cs 

    in process cs [] end
    
  val wikify : string -> string = wikify o UTF8Util.explode

  fun wikify_content content = 
      let 
        val lines = 
            String.fields (fn #"\n" => true | _ => false) content
        val sections = 
            ListUtil.tokens (StringUtil.all StringUtil.whitespec) lines

        fun process_ulpart ulpart = 
            "<li>" ^ 
            wikify (StringUtil.trim(String.concatWith "\n" (rev ulpart))) ^ 
            "</li>\n"
        fun process_ulparts [] [] ulparts =
            "<ul>\n" ^ concat (rev ulparts) ^ "</ul>\n"
          | process_ulparts [] ulpart ulparts = 
            process_ulparts [] [] (process_ulpart ulpart :: ulparts)
          | process_ulparts (line :: lines) ulpart ulparts = 
            let val line = StringUtil.trim line
            in
              if StringUtil.matchhead "*" line 
              then process_ulparts lines [String.extract(line,1,NONE)]
                                   (process_ulpart ulpart :: ulparts)
              else process_ulparts lines (line :: ulpart) ulparts
            end                 

        val process_ulparts =
         fn ls =>
            process_ulparts (tl ls)
            [String.extract(StringUtil.trim(hd ls),1,NONE)] []

        fun process_sections [] sects = String.concatWith "\n" (rev sects)
          | process_sections ([] :: lss) sects = process_sections lss sects
          | process_sections ((ls as l :: ls') :: lss) sects = 
            if StringUtil.matchhead "*" (StringUtil.trim l)
            then process_sections lss (process_ulparts ls :: sects)
            else if StringUtil.matchhead "=" l
            then process_sections (ls' :: lss) ((wikify l ^ "\n") :: sects)
            else 
              let val par = 
                      "<p>\n" ^ wikify (String.concatWith "\n" ls) ^ "\n</p>\n"
              in process_sections lss (par :: sects) end

      in process_sections sections [] end

end
