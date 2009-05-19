structure Rule = struct

  datatype pullterm = 
    LambdaP of string * pullterm
  | VarP of int * pullterm list
  | ConstP of string * pullterm list
  | EvarP of int * pullterm list

  datatype matchterm =
    LambdaM of string * matchterm
  | VarM of int * matchterm list
  | ConstM of string * matchterm list
  | StoreM of int * bool list
  | MatchM of int * pullterm list

  datatype perm = Ordered
  type rule = string vector * 
                  (string * perm * matchterm list) list * 
                  (string * perm * pullterm list) list

  fun pullterm_to_string evarnames vars needs_parens trm = 
      let 
        fun pts vars needs_parens trm = 
            case trm of 
              LambdaP(x,trm0) =>
              let val x = Names.new_name (vars,SOME x) in
                if needs_parens 
                then "(λ" ^ x ^ ". " ^
                     pts(x :: vars) false trm0 ^ ")"
                else "λ" ^ x ^ ". " ^ 
                     pts (x :: vars) false trm0 
              end
            | VarP(j,[]) => Names.nth(vars,j)
            | ConstP(c,[]) => c
            | EvarP(i,[]) => Vector.sub(evarnames,i)
            | VarP(j,trms) => 
              if needs_parens
              then
                "(" ^ Names.nth(vars,j) ^ " " ^ args vars trms ^ ")"
              else Names.nth(vars,j) ^ " " ^ args vars trms
            | ConstP(c,trms) => 
              if needs_parens
              then "(" ^ c ^ " " ^ args vars trms ^ ")"
              else c ^ " " ^ args vars trms
            | EvarP(i,trms) => 
              if needs_parens
              then "(" ^ Vector.sub(evarnames,i) ^ " " ^ args vars trms ^ ")"
              else Vector.sub(evarnames,i) ^ " " ^ args vars trms
        and args vars trms = 
            String.concatWith " " (map (pts vars true) trms)
      in pts vars needs_parens trm end
                          
  fun matchterm_to_string evarnames vars needs_parens trm = 
      let
        fun mts vars needs_parens trm = 
            case trm of 
              LambdaM(x,trm0) =>
              let val x = Names.new_name (vars,SOME x) in
                if needs_parens 
                then "(λ" ^ x ^ ". " ^
                     mts(x :: vars) false trm0 ^ ")"
                else "λ" ^ x ^ ". " ^ 
                     mts (x :: vars) false trm0 
              end
            | VarM(j,[]) => Names.nth(vars,j)
            | ConstM(c,[]) => c
            | StoreM(i,[]) => Vector.sub(evarnames,i)
            | MatchM(i,[]) => Vector.sub(evarnames,i)
            | VarM(j,trms) => 
              if needs_parens
              then
                "(" ^ Names.nth(vars,j) ^ " " ^ args vars trms ^ ")"
              else Names.nth(vars,j) ^ " " ^ args vars trms
            | ConstM(c,trms) => 
              if needs_parens
              then "(" ^ c ^ " " ^ args vars trms ^ ")"
              else c ^ " " ^ args vars trms
            | StoreM(i,trms) => 
              if needs_parens
              then "(" ^ Vector.sub(evarnames,i) ^ " " 
                   ^ String.concatWith " " vars ^ ")"
              else Vector.sub(evarnames,i) ^ " " ^ String.concatWith " " vars
            | MatchM(i,trms) => 
              if needs_parens
              then "(" ^ Vector.sub(evarnames,i) ^ " " ^ pargs vars trms ^ ")"
              else Vector.sub(evarnames,i) ^ " " ^ pargs vars trms
        and args vars trms = 
            String.concatWith " " (map (mts vars true) trms)
        and pargs vars trms = 
            String.concatWith
                " " (map (pullterm_to_string evarnames vars true) trms)
      in mts vars needs_parens trm end

  fun rule_to_string (evars,prems,concs) = 
      let
        fun prem_to_string (a,Ordered,[]) = a
          | prem_to_string (a,Ordered,[trm]) = 
            a ^ "(" ^ matchterm_to_string evars [] false trm ^ ")"
          | prem_to_string (a,Ordered,trms) = 
            a ^ " " ^ String.concatWith 
                          " " (map (matchterm_to_string evars [] true) trms)
        fun conc_to_string (a,Ordered,[]) = a
          | conc_to_string (a,Ordered,[trm]) = 
            a ^ "(" ^ pullterm_to_string evars [] false trm ^ ")"
          | conc_to_string (a,Ordered,trms) = 
            a ^ " " ^ String.concatWith 
                          " " (map (pullterm_to_string evars [] true) trms)
        val str_prems = String.concatWith " • " (map prem_to_string prems)
        val str_concs = String.concatWith " • " (map conc_to_string concs)
      in str_prems ^ " ->> " ^ str_concs end

end
