structure WikiCode = struct

  open Wiki
  open Global
  structure I = IntSyn
  val concatlines = String.concatWith "\n"

  (* === PART 1: PARSING AND UPDATING (all Ollibot, no HTML) === *)

  fun load_code (st : Signat.state,s) =
      let 
        val tokens = Parse.string_to_tokenstream s
        fun step (rules, decl) =
            case decl of 
              I.RULE rule => (rule :: rules) | _ => rules
        fun loop (st, tokens) = 
            case Parsing.parseWithStream Parse.decl_parser tokens of
              SOME(decl, tokens) =>
              let
                val st = MorphSignat.update (MorphSignat.recon(decl, st))
              in loop (st, tokens) end
            | NONE =>
              (case Stream.force tokens of
                 Stream.Nil => st
               | Stream.Cons((tok,pos),_) => 
                 raise ErrPos(pos, 
                              "Could not parse declaration beginning with " ^
                              Parse.token_to_string tok))
      in loop (st, tokens) end

  fun load_exec (st : Signat.state, s) = 
      let 
        val tokens = Parse.string_to_tokenstream s
      in
        case Parsing.parseWithStream Parse.decl_parser tokens of
          SOME(decl, tokens) =>
          let
            (* Perform type reconstruction; record any new constants *)
            val (decl, st) = MorphSignat.recon(decl, st) 
                             
            (* Load declaration into signature *)
            val st = MorphSignat.update (decl, st)
          in
            (* Run any executable content of declaration *)
            case decl of
              I.EXEC (_,n_opt,pos_prop) => 
              let val (ctx,n) = Execute.execute(pos_prop, st, n_opt)
              in (st,([ctx],n)) end
            | I.TRACE (_,n_opt,pos_prop) => 
              let fun loop (trace, n, 0) ctxs = (rev ctxs,n)
                    | loop (trace, n, m) ctxs =
                      case Stream.force trace of
                        Stream.Nil => (rev ctxs,n)
                      | Stream.Cons(ctx, trace) =>
                        loop (trace, n, m-1) (ctx :: ctxs)
                val n : int = case n_opt of NONE => 500 
                                          | SOME n => 
                                            if n > 500 then 500 else n
                val trace = Execute.trace(pos_prop, st)
              in (st, loop (trace, n, n + 1) []) end
            | _ => (st, ([],0))
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

  fun process_code (st : Signat.state,code) = 
      let val codet = "<div class=\"code\">\n" ^ preformat code ^ "</div>\n"
      in
        (load_code(st,code), codet)
        handle Global.Error(NONE,msg) => 
               (st, 
                codet ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
             | Global.Error(SOME p,msg) => 
               (st, 
                codet ^ "\n<div class=\"error\">Error: " ^ msg ^ 
                " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
             | exn => 
               (st, 
                codet ^ "\n<div class=\"error\">Unexpected error " ^
                exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")
      end

  fun process_exec (st : Signat.state,code) = 
      let val codet = "<div class=\"code\">\n" ^ preformat code ^ "</div>\n"
      in let 
          val (st,(ctxs,n)) = load_exec(st,code)
          val trace = trace_to_code (map Context.to_strings ctxs) []
        in (st, codet ^ trace) end
         handle Global.Error(NONE,msg) => 
                (st, 
                 codet ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
              | Global.Error(SOME p,msg) => 
                (st, 
                 codet ^ "\n<div class=\"error\">Error: " ^ msg ^ 
                 " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
              | exn => 
                (st, 
                 codet ^ "\n<div class=\"error\">Unexpected error " ^
                 exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")
      end

  (* === PART 3: COLLECTING AND SENDING (Interaction with server) === *)

  fun wikify_olf (send : string -> unit) content = 
      let 
        (* XXX: assumes braces at start and end of line *)
        (* XXX: comments that are *interleaved* inside of individual
         * declarations are just fucked *)
        (* XXX: we don't handle nesting *)

        val lines =
            String.fields (fn #"\n" => true | _ => false) content

        val trim = StringUtil.trim

        fun collect_code lines =
            ListUtil.partitionaslongas 
            (fn line =>
                let val s = trim line
                in not (StringUtil.matchhead "%" s)
                   andalso not (StringUtil.matchhead "--" s)
                   andalso not (StringUtil.matchhead "{" s)
                end)
            lines

        fun add_line s (xs, ys) = (s::xs, ys)

        fun starts_comment s =
            StringUtil.matchhead "{-" s orelse StringUtil.matchhead "{=" s
        fun ends_comment s =
            StringUtil.matchtail "-}" s orelse StringUtil.matchtail "=}" s

        (* aw, fuck it. nesting is not handled properly. *)
        fun collect_header_comment [] = (* this is bad *) ([], [])
          | collect_header_comment (line::lines) =
            if ends_comment (trim line) then add_line line (collect_text lines)
            else add_line line (collect_header_comment lines)
        and collect_text [] = ([], [])
          | collect_text (line::lines) =
            if StringUtil.matchhead "--" (trim line) orelse trim line = ""
            then add_line line (collect_text lines)
            else if starts_comment (trim line)
            then collect_header_comment (line::lines)
            else ([], line::lines)

        (* This isn't actually correct, since dots occur in lambdas. *)
        fun collect_percentdecl lines = 
            let val (decl,rest) = 
                ListUtil.partitionaslongas 
                    (fn line =>
                        not (StringUtil.matchtail "." (trim line)))
                    lines
            in 
              case rest of 
                [] => (decl, []) (* Definitely an error at this point! *)
              | line :: rest => (decl @ [line], rest)
            end

        fun run st [] = ()
          | run st (lines as line :: _) =
            let val line = trim line
            in
                if StringUtil.matchhead "%" line
                then run_percentdecl st lines
                else if StringUtil.matchhead "--" line
                        orelse StringUtil.matchhead "{" line
                then run_text st lines
                else if StringUtil.trim line = ""
                then run_text st lines
                else run_code st lines
            end

        and run_code st lines = 
            let
              val (code,lines) = collect_code lines
              val (st,output) = 
                  process_code(st, StringUtil.trim(concatlines code))
            in send output; run st lines end

        and run_text st lines = 
            let
              val (text,lines) = collect_text lines
              val trimgarbage =
                  StringUtil.losespecl
                      (fn c => StringUtil.whitespec c orelse c = #"-"
                               orelse c = #"{") o
                  StringUtil.losespecr
                      (fn c => StringUtil.whitespec c orelse c = #"-"
                               orelse c = #"}")
              val text = concatlines (map trimgarbage text)
            in send (wikify_content text); run st lines end

        and run_percentdecl st lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val (st,output) = 
                  process_exec(st, StringUtil.trim(concatlines decl))
            in send output; run st lines end

      in
        run Signat.empty lines
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

        fun run (st : Signat.state) [] = ()
          | run st (lines as line :: _) =
            if not (StringUtil.matchhead ">" line)
            then run_text st lines
            else if StringUtil.matchhead "%"
                       (StringUtil.trim (String.extract(line,1,NONE)))
            then run_percentdecl st lines
            else run_code st lines

        and gather code = 
            concatlines (map (fn line => (String.extract(line,2,NONE))) code)

        and run_code (st : Signat.state) lines = 
            let
              val (code,lines) = collect_code lines
              val (st,output) = process_code(st, gather code)
            in send output; run st lines end

        and run_text (st : Signat.state) lines = 
            let
              val (text,lines) = collect_text lines
              val text = concatlines text
            in send (wikify_content text); run st lines end

        and run_percentdecl (st : Signat.state) lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val (st,output) = process_exec(st, gather decl)
            in send output; run st lines end

      in
        run Signat.empty lines
      end


end
