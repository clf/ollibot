structure Context = struct

  datatype context = 
      S of {persistent: (string * Term.term list) list,
            linear: (string * Term.term list) list,
            ordered: (string * Term.term list) list}

  fun to_string (S{ordered,...}) = 
      let 
        fun mapper (s,[]) = s
          | mapper (s,[trm]) = s ^ "(" ^ Term.to_string trm ^ ")"
          | mapper (s,trms) = 
            s ^ " " ^ String.concatWith " " (map Term.to_string trms)
      in String.concatWith " • " (map mapper ordered) end

end
