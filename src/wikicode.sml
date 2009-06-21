structure WikiCode = struct

  open Wiki
  open Global
  structure I = IntSyn
  val concatlines = String.concatWith "\n"

  type state = I.tp MapS.map * I.rule list

  (* === PART 1: PARSING AND UPDATING (all Ollibot, no HTML) === *)

  fun load_code (state,s) =
      let 
        val tokens = Parse.string_to_tokenstream s
        fun step (rules, decl) =
            case decl of 
              I.RULE rule => (rule :: rules) | _ => rules
        fun loop ((signat,rules), tokens) = 
            case Parsing.parseWithStream Parse.decl_parser tokens of
              SOME(decl, tokens) =>
              let
                val (signat, decl) = TypeRecon.load_decl (signat, decl)
                val rules = step (rules, decl)
              in loop ((signat, rules), tokens) end
            | NONE =>
              (case Stream.force tokens of
                 Stream.Nil => (signat,rules)
               | Stream.Cons((tok,pos),_) => 
                 raise ErrPos(pos, 
                              "Could not parse declaration beginning with " ^
                              Parse.token_to_string tok))
      in loop (state,tokens) end

  fun load_exec ((signat,rules), s) = 
      let 
        val tokens = Parse.string_to_tokenstream s
      in
        case Parsing.parseWithStream Parse.decl_parser tokens of
          SOME(decl, tokens) =>
          let
            val (signat, decl) = TypeRecon.load_decl (signat, decl)
          in 
            case decl of 
              I.RULE rule => ((signat,rule :: rules),([],0))
            | I.EXEC (p,n,pos_prop) => 
              let val (ctx,n) = Execute.execute(pos_prop, rules, n)
              in ((signat, rules),([ctx],n)) end
            | I.TRACE (p,n_opt,pos_prop) => 
              let fun loop (trace, n, 0) ctxs = (rev ctxs,n)
                    | loop (trace, n, m) ctxs =
                        case Stream.force trace of
                          Stream.Nil => (rev ctxs,n)
                        | Stream.Cons(ctx, trace) =>
                          loop (trace, n, m-1) (ctx :: ctxs)
                val n : int = case n_opt of NONE => 500 
                                          | SOME n => 
                                            if n > 500 then 500 else n
                val trace = Execute.trace(pos_prop, rules)
              in ((signat,rules), loop (trace,n,n+1) []) end
          end
        | NONE => raise Err("Could not read a declaration!")
      end

  (* === PART 2: FORMATTING AND ERROR REPORTING (Only code) === *)

  fun trace_to_code cs lines = 
      case cs of 
        [] => "<div class=\"hyps\">\n" ^ 
              concat(rev lines) ^ 
              "<div class=\"hyp\"></div>\n</div>\n" 
      | {persistent,linear,ordered} :: cs =>
        let 
          fun spanify perm atom =
              "<span class=\"hyp_" ^ perm ^ "\">" ^
              preformat atom ^ "</span>"
          val atoms = 
              map (fn s => spanify "pers" s) persistent @
              map (fn s => spanify "lin" s) linear @
              map (fn s => spanify "ord" s) ordered
          val line = 
              "<div class=\"hyp\">" ^ 
              String.concatWith "<span class=\"hyp_space\">•</span>" atoms ^
              "</div>\n"
        in
          trace_to_code cs (line :: lines)
        end

  fun process_code (state : state,code) = 
      let val codet = "<div class=\"code\">\n" ^ preformat code ^ "</div>\n"
      in
        (load_code(state,code), codet)
        handle Global.Error(NONE,msg) => 
               (state, 
                codet ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
             | Global.Error(SOME p,msg) => 
               (state, 
                codet ^ "\n<div class=\"error\">Error: " ^ msg ^ 
                " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
             | exn => 
               (state, 
                codet ^ "\n<div class=\"error\">Unexpected error " ^
                exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")
      end

  fun process_exec (state,code) = 
      let val codet = "<div class=\"code\">\n" ^ preformat code ^ "</div>\n"
      in let 
          val (state,(ctxs,n)) = load_exec(state,code)
          val trace = trace_to_code (map Context.to_strings ctxs) []
        in (state, codet ^ trace) end
         handle Global.Error(NONE,msg) => 
                (state, 
                 codet ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
              | Global.Error(SOME p,msg) => 
                (state, 
                 codet ^ "\n<div class=\"error\">Error: " ^ msg ^ 
                 " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
              | exn => 
                (state, 
                 codet ^ "\n<div class=\"error\">Unexpected error " ^
                 exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")
      end

  (* === PART 3: COLLECTING AND SENDING (Interaction with server) === *)

  fun wikify_olf (send : string -> unit) content = 
      let 
        val lines =
            String.fields (fn #"\n" => true | _ => false) content

        fun collect_code lines =
            ListUtil.partitionaslongas 
            (fn line => not (StringUtil.matchhead "%" (StringUtil.trim line)))
            lines

        fun collect_text lines = 
            ListUtil.partitionaslongas
            (fn line => 
                StringUtil.matchhead "%%" (StringUtil.trim line)
                orelse StringUtil.trim line = "") 
            lines

        fun collect_percentdecl lines = 
            let val (decl,rest) = 
                ListUtil.partitionaslongas 
                    (fn line =>
                        not (StringUtil.matchtail "." (StringUtil.trim line)))
                    lines
            in 
              case rest of 
                [] => (decl, []) (* Definitely an error at this point! *)
              | line :: rest => (decl @ [line], rest)
            end

        fun run state [] = ()
          | run state (lines as line :: _) =
            if StringUtil.matchhead "%" (StringUtil.trim line)
            then (if StringUtil.matchhead "%%" (StringUtil.trim line)
                  then run_text state lines
                  else run_percentdecl state lines)
            else if StringUtil.trim line = ""
            then run_text state lines
            else run_code state lines

        and run_code state lines = 
            let
              val (code,lines) = collect_code lines
              val (state,output) = 
                  process_code(state, StringUtil.trim(concatlines code))
            in send output; run state lines end

        and run_text state lines = 
            let
              val (text,lines) = collect_text lines
              val trimpercent =
                  StringUtil.losespecl 
                      (fn c => StringUtil.whitespec c orelse c = #"%") 
              val text = concatlines (map trimpercent text)
            in send (wikify_content text); run state lines end

        and run_percentdecl state lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val (state,output) = 
                  process_exec(state, StringUtil.trim(concatlines decl))
            in send output; run state lines end

      in
        run (MapS.empty, []) lines
      end

  fun wikify_lolf (send : string -> unit) content = 
      let 
        val lines =
            String.fields (fn #"\n" => true | _ => false) content

        fun collect_code lines = 
            ListUtil.partitionaslongas
            (fn line => StringUtil.matchhead ">" line
                        andalso 
                        not (StringUtil.matchhead "%" 
                              (StringUtil.trim (String.extract(line,1,NONE)))))
            lines

        fun collect_text lines = 
            ListUtil.partitionaslongas
            (fn line => 
                not (StringUtil.matchhead ">" (StringUtil.trim line))
                orelse StringUtil.trim line = "") 
            lines

        fun collect_percentdecl lines = 
            let val (decl,rest) = 
                ListUtil.partitionaslongas 
                    (fn line =>
                        StringUtil.matchhead ">" line
                        andalso 
                        not (StringUtil.matchtail "." (StringUtil.trim line)))
                    lines
            in 
              case rest of 
                [] => (decl, []) (* Definitely an error at this point! *)
              | line :: rest => (decl @ [line], rest)
            end

        fun run state [] = ()
          | run state (lines as line :: _) =
            if not (StringUtil.matchhead ">" line)
            then run_text state lines
            else if StringUtil.matchhead "%"
                       (StringUtil.trim (String.extract(line,1,NONE)))
            then run_percentdecl state lines
            else run_code state lines

        and gather code = 
            concatlines (map (fn line => (String.extract(line,2,NONE))) code)

        and run_code state lines = 
            let
              val (code,lines) = collect_code lines
              val (state,output) = process_code(state, gather code)
            in send output; run state lines end

        and run_text state lines = 
            let
              val (text,lines) = collect_text lines
              val text = concatlines text
            in send (wikify_content text); run state lines end

        and run_percentdecl state lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val (state,output) = process_exec(state, gather decl)
            in send output; run state lines end

      in
        run (MapS.empty, []) lines
      end


end
