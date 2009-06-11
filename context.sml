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
            p ^ s ^ " " ^ String.concatWith " " (map Term.to_string trms)
      in 
        String.concatWith " • " (map (mapper "!") persistent @
                                 map (mapper "¡") linear @
                                 map (mapper "") ordered)
      end

end
