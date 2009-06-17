structure Trans = struct

  val translate = 
      String.translate
          (fn c => if ord c > 127 then "\\" ^ Int.toString (ord c) else str c)

  fun translate_streams(instream,outstream) = 
      let 
        fun loop () = 
            case TextIO.inputLine instream of
              NONE =>
              (TextIO.closeIn instream; 
               TextIO.closeOut outstream)
            | (SOME s) =>
              (TextIO.output(outstream, translate s); loop())
      in loop() end
        
  fun translate_dir(indir,outdir,dirstream) = 
      let
        fun loop () = 
            case OS.FileSys.readDir dirstream of
              NONE => ()
            | SOME(file) =>
              let val ext = OS.Path.ext file
              in 
                if ext = SOME "sml" orelse ext = SOME "mlb"
                then
                  let 
                    val infile = OS.Path.joinDirFile{dir=indir,file=file}
                    val outfile = OS.Path.joinDirFile{dir=outdir,file=file}
                    val instream = TextIO.openIn infile
                    val outstream = TextIO.openOut outfile
                  in translate_streams(instream,outstream) end
                else ();
                loop()
              end
      in loop() end

  val start = 
      let 
        val dirstream1 = OS.FileSys.openDir "src"
        val dirstream2 = OS.FileSys.openDir "src/parse"
        val dirstream3 = OS.FileSys.openDir "src/util"
        val dirstream4 = OS.FileSys.openDir "src/server"
      in
        OS.FileSys.mkDir "escape-src";
        translate_dir ("src","escape-src",dirstream1);
        OS.FileSys.mkDir "escape-src/parse";
        translate_dir ("src/parse","escape-src/parse",dirstream2);
        OS.FileSys.mkDir "escape-src/util";
        translate_dir ("src/util","escape-src/util",dirstream3);
        OS.FileSys.mkDir "escape-src/server";
        translate_dir ("src/server","escape-src/server",dirstream4);
        ()
      end        

end
