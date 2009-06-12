structure Names = struct

  fun new_name (vars,x) =
      let 
        fun new_name_backup (vars,x,i) = 
            if List.exists (fn y => (x ^ Int.toString i) = y) vars
            then new_name_backup (vars,x,i+1)
            else x ^ Int.toString i
      in 
        if List.exists (fn y => x = y) vars 
        then new_name_backup (vars,x,1) else x
      end

  fun new_name_opt (vars,x) = 
      case x of 
        NONE => new_name (vars,"x")
      | SOME x => new_name (vars,x)


  fun nth (vars,j) = 
      if length vars > j then List.nth(vars,j)
      else "xx" ^ Int.toString(j - length vars)
           
end

