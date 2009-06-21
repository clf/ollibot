structure OllibotWebHandler :> WEB_HANDLER = struct

  open OS
  open DefaultHandler
  val DEFAULT_PORT = 8000
  val ROOT_DIR = "examples"

  exception NotFound
  fun get_file url = 
      let val file = ROOT_DIR ^ url
      in
        (* Security checks *)
        if isSome (StringUtil.find ".." url) then raise NotFound 
        else if isSome (StringUtil.find ";" url) then raise NotFound
        (* Try to open file; raise NotFound if it doesn't work *) 
        else if FileSys.isDir file
        then 
          let val file = Path.joinDirFile {dir=file, file="readme.txt"}
          in (file, TextIO.openIn file) end
        else (file, TextIO.openIn file)
      end handle _ => raise NotFound

  val header = 
   fn title => 
( "<html><head><title>Ollibot : " ^ title ^ "</title></head>\n"
^ "<body>\n"
^ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
^ "<!DOCTYPE html \n"
^ "     PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
^ "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
^ "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"
^ "\n"
^ "<head>\n"
^ "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n"
^ "<title>Tiny Ollibot Example Server - Call-by-need functions</title>\n"
^ "<link rel=\"stylesheet\" type=\"text/css\" href=\"/ollibot.css\" media=\"screen,projection\" />\n"
^ "</head>\n" 
^ "\n"  
^ "<body>\n"
^ " <div id=\"main\">\n")

  fun get_title content = 
      let
        val content =
            StringUtil.losespecl
                (fn #"%" => true | #"=" => true | c => StringUtil.whitespec c)
                content
        val (title,_) =
            StringUtil.token
                (fn #"%" => true | #"=" => true | #"\n" => true | _ => false)
                content
      in StringUtil.trim title end

  fun txt_page (file,f) =
      let 
        val send : string -> unit = f (OK_200, "text/html; charset=utf-8")
        val content = TextIO.inputAll file
      in 
        send (header (get_title content));
        send (Wiki.wikify_content content); 
        send " </div>\n</body>\n</html>\n"
      end

  fun lolf_page (file,f) = 
      let
        val send : string -> unit = f (OK_200, "text/html; charset=utf-8")
        val content = TextIO.inputAll file
      in 
        send (header (get_title content));
        WikiCode.wikify_lolf send content; 
        send " </div>\n</body>\n</html>\n"
      end

  fun olf_page (file,f) = 
      let
        val send : string -> unit = f (OK_200, "text/html; charset=utf-8")
        val content = TextIO.inputAll file
      in 
        send (header (get_title content));
        WikiCode.wikify_olf send content; 
        send " </div>\n</body>\n</html>\n"
      end

  fun css_page (file,f) = 
      let
        val send : string -> unit = f (OK_200, "text/css; charset=utf-8")
        val content = TextIO.inputAll file
      in send content end

  fun build_reply {method,url,headers,lookup} f = 
      let
        val (filename,file) = get_file url
        val {base,ext} = Path.splitBaseExt filename
      in
        case ext of 
          SOME "txt" => (print "txt..."; txt_page (file, f))
        | SOME "css" => (print "css..."; css_page (file, f))
        | SOME "olf" => (print "olf..."; olf_page (file, f))
        | SOME "lolf" => (print "lolf..."; lolf_page (file, f))
        | SOME s => (print("unknown: " ^ s ^ "..."); not_found_page f)
        | NONE => (print "no extension..."; not_found_page f);
        print "done..."
      end
      handle NotFound => (print "error..."; not_found_page f)
           | exn => (print ("Unexpected error " ^ exnName exn ^ "\n" ^
                            exnMessage exn ^ "\n"))

end
