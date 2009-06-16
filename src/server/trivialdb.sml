
(* This trivial database simply provides a persistent key-value pair mapping,
   where both keys and values are strings. *)

structure TrivialDB :> TRIVIALDB =
struct
  
  exception TrivialDB of string

  val dbfile = Params.param "tdb.txt"
        (SOME("-tdb",
              "What file to use for the trivial persistent database"))
        "trivialdb"

  structure SM = StringMap

  val db = ref SM.empty : { value : string, hooks : (unit -> bool) list } SM.map ref

  fun init () =
    let
      fun dec s = 
        case StringUtil.urldecode s of
          NONE => raise TrivialDB "bad urlencoded string"
        | SOME k => k

      val dbt = StringUtil.readfile (!dbfile)
      val lines = String.tokens (StringUtil.ischar #"\n") dbt
      fun oneline s =
        case String.fields (StringUtil.ischar #" ") s of
          [key, value] => 
            let in
              print ("DB: " ^ dec key ^ " --> " ^ dec value ^ "\n");
              db := SM.insert(!db, dec key, { value = dec value,
                                              hooks = nil })
            end
        | _ => raise TrivialDB "Bad line in database!\n"
    in
      app oneline lines
    end handle Io => 
      let in
        print "Warning: database file not found; starting empty\n";
        db := SM.empty
      end

  (* Hooks cannot update the data or add hooks. *)
  exception Reentrant

  val reentrant = ref false
  fun update key value =
    if !reentrant 
    then raise Reentrant
    else
      let 
        (* first update database *)
        val () = reentrant := true
        val runhooks = (case SM.find (!db, key) of
                          NONE => nil
                        | SOME { value = _, hooks } => hooks)
        (* hooks can read the db, so make sure it is consistent *)
        val () = db := SM.insert(!db, key, { value = value, hooks = nil })

        (* keep the hooks that return true *)
        val keephooks = List.filter (fn f => f ()) runhooks


      in
        db := SM.insert(!db, key, { value = value, hooks = keephooks });

        (* PERF ridiculous to write to disk on every update *)
        let     val f = TextIO.openOut (!dbfile)
        in
          SM.appi (fn (k, { value, hooks = _ }) =>
                   let in
                     TextIO.output(f, StringUtil.urlencode k);
                     TextIO.output(f, " ");
                     TextIO.output(f, StringUtil.urlencode value);
                     TextIO.output(f, "\n")
                   end) (!db);
          TextIO.closeOut f
        end;
        
        reentrant := false
      end

  fun read key =
    case SM.find (!db, key) of
      NONE => ""
    | SOME { value, ... } => value

  fun addhook key f =
    if !reentrant 
    then raise Reentrant
    else
      let
        val (v, h) = (case SM.find (!db, key) of
                        NONE => ("", nil)
                      | SOME { value, hooks } => (value, hooks))
      in
        db := SM.insert(!db, key, { value = v, hooks = f :: h })
      end

end
