structure EmacsInteract = struct

fun run() = 
    let
      val input = TextIO.inputLine TextIO.stdIn
      val p = print
    in
      case input of 
        SOME("del\n") => 
        (p "(progn\n";
         p "(delete-overlay a-overlay)\n";
         p "(delete-overlay b-overlay)\n";
         p "'Clear-completed)\n";
         run())
      | SOME(_) => 
        (p "(progn\n";
         p "(setq a-buffer (get-buffer \"chars2.txt\"))\n";
         p "(setq a-overlay (make-overlay 1 5 a-buffer))\n";
         p "(setq b-overlay (make-overlay 5 9 a-buffer))\n";
         p "(overlay-put a-overlay 'face '((foreground-color . \"red\")))\n";
         p "(overlay-put b-overlay 'face '((foreground-color . \"blue\")))\n";
         p "'Highlight-completed)\n";
         run())
      | NONE => 
        ()
    end

val a = run()

end
