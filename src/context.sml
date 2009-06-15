signature CONTEXT = sig

  datatype context = 
      S of {persistent: (string * Term.term list) list,
            linear: (string * Term.term list) list,
            ordered: (string * Term.term list) list}

  val to_string : context -> string

  val to_web_string : context -> string

end

structure Context :> CONTEXT = struct

  open Global

  datatype context = 
      S of {persistent: (string * Term.term list) list,
            linear: (string * Term.term list) list,
            ordered: (string * Term.term list) list}

  fun to_string (S{ordered,linear,persistent}) = 
      let 
        fun mapper p (s,[]) = p ^ s
          | mapper p (s,[trm]) = p ^ s ^ "(" ^ Term.to_string trm ^ ")"
          | mapper p (s,trms) = 
            p ^ s ^ " " ^ String.concatWith " " (map Term.to_string_parens trms)
      in 
        String.concatWith " • " (map (mapper "!") persistent @
                                 map (mapper "¡") linear @
                                 map (mapper "") ordered)
      end

  fun to_web_string (S{ordered,linear,persistent}) = 
      let 
        fun mapper pre post (s,trms) = 
            case trms of 
              [] => pre ^ "<span class=\"hyp_pred\">" ^ s ^ "</span>" ^ post
            | [trm] =>
              pre ^ "<span class=\"hyp_pred\">" ^ s ^ "</span>" ^ 
              "(" ^ Term.to_string trm ^ ")" ^ post
            | trms =>
              pre ^ "<span class=\"hyp_pred\">" ^ s ^ "</span> " ^ 
              String.concatWith " " (map Term.to_string_parens trms) ^ post
      in 
        "<div class=\"hyp\">" ^
        String.concatWith ("<span class=\"hyp_space\">•</span>")
           (map (mapper "<span class=\"hyp_pers\">!" "</span>") persistent @
            (*if null persistent then [] 
             else ["<span class=\"hyp_space\">&nbsp;</span>"]*) 
            map (mapper "<span class=\"hyp_lin\">¡" "</span>") linear @
            (*if null linear then [] 
             else ["<span class=\"hyp_space\">&nbsp;</span>"]*) 
            map (mapper "<span class=\"hyp_ord\">" "</span>") ordered) ^
        "</div>\n"
      end

end
