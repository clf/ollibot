structure Context = struct

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
              [] => pre ^ s ^ post
            | [trm] => pre ^ s ^ "(" ^ Term.to_string trm ^ ")" ^ post
            | trms =>
              pre ^ s ^ " " ^ 
              String.concatWith " " (map Term.to_string_parens trms) ^ post
      in 
        "\n  <div id=\"hyp\">\n" ^
        String.concatWith "\n"
           (map (mapper "   <span id=\"hyp_pers\">" "</span>") persistent @
            map (mapper "   <span id=\"hyp_lin\">" "</span>") linear @
            map (mapper "   <span id=\"hyp_ord\">" "</span>") ordered) ^
        "\n  </div>\n"
      end

end
