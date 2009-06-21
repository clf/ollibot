structure WebTools = struct

  open Wiki

  fun code_update (state,code) = 
      let val codet = preformat code
      in
        (Webend.read(state,s), codeat)
      end
      handle Global.Error(NONE,msg) => 
             (state, 
              codeat ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
           | Global.Error(SOME p,msg) => 
             (state, 
              codeat ^ "\n<div class=\"error\">Error: " ^ msg ^ 
              " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
           | exn => 
             (state, 
              codeat ^ "\n<div class=\"error\">Unexpected error " ^
              exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")

  fun exec_update (state,code) = 
      let val codet = preformat code
      in
        (Webend.exec(state,s), codeat)
      end
      handle Global.Error(NONE,msg) => 
             ((state,(0,[])), 
              codeat ^ "\n<div class=\"error\">Error: " ^ msg ^ "</div>\n")
           | Global.Error(SOME p,msg) => 
             ((state,(0,[])),
              codeat ^ "\n<div class=\"error\">Error: " ^ msg ^ 
              " on line " ^ Int.toString(Pos.getline p) ^ "</div>\n")
           | exn => 
             ((state,(0,[])),
              codeat ^ "\n<div class=\"error\">Unexpected error " ^
              exnName exn ^ ": " ^ exnMessage exn ^ "</div>\n")

  fun trace_to_code cs lines = 
      case cs of 
        [] => rev lines
      | {persistent,linear,ordered} :: cs =>
        let 
          fun spanify perm atom =
              "<span class=\"hyp_" ^ perm ^ "\">" ^
              preformat atom ^ "</span>"
          val atoms = 
              map (fn s => spanify "perm" s) persistent @
              map (fn s => spanify "lin" s) linear @
              map (fn s => spanify "ord" s) ordered
          val line = 
              "<div class=\"hyp\">" ^ 
              String.concatWith "<span class=\"hyp_space\">•</span>" atoms ^
              "</div>\n"
        in trace_to_code cs (line :: lines) end

  fun wikify_olf send content = 
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

        fun process state [] = ()
          | process state (lines as line :: _) =
            if StringUtil.matchhead "%" (StringUtil.trim line)
            then (if StringUtil.matchhead "%%" (StringUtil.trim line)
                  then process_text state lines
                  else process_percentdecl state lines)
            else if StringUtil.trim line = ""
            then process_text state lines
            else process_code state lines

        and process_code state lines = 
            let
              val (code,lines) = collect_code lines
              val code = String.concatWith "\n" code
              val code = preformat code
              val () = send "<div class=\"code\">\n"
              val () = send code
              val () = send "</div>"
            in process state lines end

        and process_text state lines = 
            let
              val (text,lines) = collect_text lines
              val trimpercent =
                  StringUtil.losespecl 
                      (fn c => StringUtil.whitespec c orelse c = #"%") 
              val text = 
                  String.concatWith "\n" (map trimpercent text)
              val () = send (wikify_content text)
            in process state lines end

        and process_percentdecl state lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val decl = String.concatWith "\n" decl
              val decl = preformat decl
              val () = send "<div class=\"exec\">\n"
              val () = send decl
              val () = send "</div>"
            in process state lines end

      in
        process (MapS.empty, []) lines
      end

  fun wikify_lolf send content = 
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

        fun process state [] = ()
          | process state (lines as line :: _) =
            if not (StringUtil.matchhead ">" line)
            then process_text state lines
            else if StringUtil.matchhead "%"
                       (StringUtil.trim (String.extract(line,1,NONE)))
            then process_percentdecl state lines
            else process_code state lines

        and process_code state lines = 
            let
              val (code,lines) = collect_code lines
              val code = map 
                         (fn line => (String.extract(line,2,NONE)))
                         code
              val code = String.concatWith "\n" code
              val code = preformat code
              val () = send "<div class=\"code\">\n"
              val () = send code
              val () = send "</div>"
            in process state lines end

        and process_text state lines = 
            let
              val (text,lines) = collect_text lines
              val text = String.concatWith "\n" text
              val () = send (wikify_content text)
            in process state lines end

        and process_percentdecl state lines = 
            let 
              val (decl,lines) = collect_percentdecl lines
              val decl = map 
                         (fn line => 
                             StringUtil.trim (String.extract(line,2,NONE)))
                         decl
              val decl = String.concatWith "\n" decl
              val decl = preformat decl
              val () = send "<div class=\"exec\">\n"
              val () = send decl
              val () = send "</div>"
            in process state lines end

      in
        process (MapS.empty, []) lines
      end


end
