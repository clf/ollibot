signature CONTEXT = sig

  datatype context = 
      S of {persistent: (string * Term.term list) list,
            linear: (string * Term.term list) list,
            ordered: (string * Term.term list) list}

  val to_string : context -> string

  val to_strings : context ->
      {persistent: string list, linear: string list, ordered: string list}

  val to_web_string : context -> string

end

structure Context :> CONTEXT = struct

  open Global

  datatype context = 
      S of {persistent: (string * Term.term list) list,
            linear: (string * Term.term list) list,
            ordered: (string * Term.term list) list}

  fun mapper pre post (s,trms) = 
      case trms of 
        [] => pre ^ s ^ post
      | [trm] => pre ^ s ^ "(" ^ Term.to_string trm ^ ")" ^ post
      | trms => pre ^ s ^ " " ^
                String.concatWith " " (map Term.to_string_parens trms) ^ post

  fun to_string (S{ordered,linear,persistent}) = 
      String.concatWith ",\n   " (map (mapper "!" "") persistent @
                               map (mapper "" "") linear @
                               map (mapper "?" "") ordered)
      
  fun to_strings (S{ordered,linear,persistent}) = 
      {persistent = map (mapper "!" "") persistent,
       linear = map (mapper "" "") linear,
       ordered = map (mapper "?" "") ordered}

  fun to_web_string (S{ordered,linear,persistent}) = 
      "<div class=\"hyp\">" ^
      String.concatWith ("<span class=\"hyp_space\">, </span>")
           (map (mapper "<span class=\"hyp_pers\">!" "</span>") persistent @
            map (mapper "<span class=\"hyp_lin\">" "</span>") linear @
            map (mapper "<span class=\"hyp_ord\">?" "</span>") ordered) ^
        "</div>\n"

end
