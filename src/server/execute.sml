
structure Execute :> EXECUTE =
struct

  infixr 9 `
  fun a ` b = a b

  structure B = Bytecode
  structure Q = Queue
  structure P = Primop
  structure SM = StringMap

  exception Execute of string

  (* a paused thread, ready to execute. *)
  type thread = { global : int * int,
                  (* rep invt.: all values *)
                  args : B.exp list }  

  datatype instance =
    I of { prog : B.program,
           locals : Marshal.locals,
           active : bool ref,
           threads : thread Q.queue ref,
           messages : string Q.queue ref }

  (* no threads, messages... *)
  fun new p = I { prog = p, 
                  active = ref true, 
                  locals = Marshal.new (), 
                  threads = ref ` Q.empty (), 
                  messages = ref ` Q.empty () }

  fun destroy (I { active, ... }) = active := false

  fun addmessage (i as I { messages, ... }) x = messages := Q.enq(x, !messages)
  fun getlocals  (I { locals, ... }) = locals

  val btrue = B.Inj ("true", NONE)
  val bfalse = B.Inj ("false", NONE)

  fun step (i as I { prog, threads, ... }) =
    (* if there are threads, then do some work on the first one *)
    case Q.deq (!threads) of
      (NONE, _) => false
    | (SOME { global = (g, f), args }, q') => 
        let in
          threads := q';
          (* print ("(Step) do thread " ^ Int.toString g ^ "." ^ Int.toString f ^ "..\n"); *)
          (* Threads should only jump to a FunDec; a continuation that does not return.
             OneDecs (which are compiled AllLams) return values, and are invoked with Call.
             Absent means that this label only exists on specific other worlds. *)
          (case Vector.sub (#globals prog, g) of
             B.FunDec v =>
               let 
                 val (params, stmt) = Vector.sub (v, f)
                 val binds = ListUtil.wed params args handle _ => raise Execute "wrong number of args"
                 val G = SM.empty
                 val G = foldr (fn ((p, a), G) =>
                                SM.insert(G, p, a)) G binds
               in
                 case execute i G stmt of
                   NONE => ()
                 | SOME _ => raise Execute "funcall returned result ?!"
               end
           | B.OneDec _ => raise Execute "thread jumped to onedec?"
           | B.Absent => raise Execute "thread jumped out of this world!");
          true
        end

  (* execute a statement, returning its result, if any. *)
  and execute (i as I { threads, ... } : instance) 
              (G : B.exp SM.map) (stmt : B.statement) : B.exp option =
    case stmt of
      B.End => NONE
    | B.Return e => SOME (evaluate i G e)
    | B.Bind (s, e, st) =>
        let 
        in
          (* print ("bind " ^ s ^ "\n"); *)
          execute i (SM.insert(G, s, evaluate i G e)) st
        end
    | B.Case { obj, var, arms, def } =>
        (case evaluate i G obj of
           B.Inj (l, e) =>
               (case ListUtil.Alist.find op= arms l of
                    NONE => execute i G def
                  | SOME arm => execute i (case e of 
                                             NONE => G
                                           | SOME e => SM.insert(G, var, e)) arm)
         | _ => raise Execute "case obj not sum")

    | B.Jump (eg, ef, args) =>
        (case (evaluate i G eg, 
               evaluate i G ef,
               map (evaluate i G) args) of
           (B.Int g, B.Int f, args) => 
             let in
               threads := Q.enq ({ global = (IntConst.toInt g, 
                                             IntConst.toInt f),
                                   args = args }, !threads);
               NONE
             end
         | _ => raise Execute "jump needs two ints, args")

    | B.Go (addr, bytes) =>
           (case (evaluate i G addr, evaluate i G bytes) of
              (B.String addr, B.String bytes) =>
                let in 
                  (if addr = Worlds.server
                   then come i bytes
                   else if addr = Worlds.home 
                        then addmessage i bytes
                        else raise Execute ("unrecognized world " ^ addr));
                  NONE
                end
            | _ => raise Execute "go needs two strings")

    | B.Error s => raise Execute ("error: " ^ s)

  and evaluate (i : instance) (G : B.exp SM.map) (exp : B.exp) =
    case exp of
      B.Int _ => exp
    | B.String _ => exp
    | B.Ref _ => exp
    | B.Inj (l, e) => B.Inj (l, Option.map (evaluate i G) e)
    | B.Record lel => B.Record ` ListUtil.mapsecond (evaluate i G) lel
    | B.Project (l, e) =>
        (case evaluate i G e of
           B.Record lel => 
             (case ListUtil.Alist.find op= lel l of
                NONE => raise Execute ("label not found: " ^ l)
              | SOME e => e)
         | _ => raise Execute ("project from non-record: label " ^ l))

    | B.Marshal (ed, ee) =>
        let 
          val (vd, ve) = (evaluate i G ed, evaluate i G ee)
          val s = Marshal.marshal (getlocals i) vd ve
        in
          B.String s
        end

    | B.Call (e, args) =>
        (case (i, evaluate i G e, map (evaluate i G) args) of
           (I { prog, ...}, B.Int l, args) =>
             (case Vector.sub (#globals prog, IntConst.toInt l) of
                B.OneDec (vargs, st) =>
                  let
                    val () = if length vargs <> length args
                             then raise Execute "call argnum mismatch"
                             else ()
                    (* closed. totally new environment: *)
                    val G = foldl (fn ((v, a), G) => SM.insert(G, v, a)) SM.empty
                          ` ListPair.zip (vargs, args)
                  in
                    case execute i G st of
                      NONE => raise Execute "call didn't return?"
                    | SOME v => v
                  end
              | B.FunDec _ => raise Execute "call to fundec?"
              | B.Absent => raise Execute "call out of this world!")
          | _ => raise Execute "call to non-label") 

    | B.Primop (po, args) => 
        (case (po, map (evaluate i G) args) of
           (P.PRef, [v]) => B.Ref ` ref v
         | (P.PRef, _) => raise Execute "bad pref"
         | (P.PGet, [B.Ref (ref v)]) => v
         | (P.PGet, _) => raise Execute "bad pget"
         | (P.PSet, [B.Ref r, v]) => (r := v; B.Record nil)
         | (P.PSet, _) => raise Execute "bad pset"
         | (P.PEqs, [B.String a, B.String b]) => 
             if a = b then btrue else bfalse

         | (P.B bop, [B.Int x, B.Int y]) => 
             (case bop of
                P.PCmp cmp =>
                  if (case cmp of
                        P.PEq => op=
                      | P.PNeq => op<>
                      | P.PLess => op<
                      | P.PLesseq => op<=
                      | P.PGreater => op>
                      | P.PGreatereq => op>=) (x, y) 
                  then btrue else bfalse
                (* the rest return ints *)
              | _ =>
                    B.Int 
                    ((case bop of
                        P.PTimes => op*
                      | P.PPlus  => op+
                      | P.PMinus => op-
                      | P.PDiv   => op div
                      | P.PMod   => op mod
(*
                      | PAndb  => IntConst.andb
                      | PXorb  => IntConst.xorb
                      | POrb   => IntConst.orb
*)
                      (* shl/shr *)
                      | _ => raise Execute "unimplemented binop") 
                        (x, y)))

         | (P.PIntToString, [B.Int i]) => B.String (Int.toString (IntConst.toInt i))
         | (P.PIntToString, _) => raise Execute "bad inttostring"

         | (P.PStringSub, [B.String s, B.Int i]) =>
             ( B.Int (IntConst.fromInt ` ord (String.sub(s, IntConst.toInt i))) handle _ => raise Execute "string sub out of bounds" )
         | (P.PStringSub, _) => raise Execute "bad stringsub"
               
         | (P.PStringSubstring, [B.String s, B.Int i, B.Int n]) =>
             ( B.String (String.substring(s, IntConst.toInt i, IntConst.toInt n)) 
               handle _ => raise Execute "string sub out of bounds" )
         | (P.PStringSubstring, _) => raise Execute "bad stringsubstring"
         | (P.PStringLength, [B.String s]) => B.Int (IntConst.fromInt (size s))
         | (P.PStringLength, _) => raise Execute "bad stringlength"
         | (P.PStringReplace, [B.String s, B.String d, B.String t]) =>
               B.String (StringUtil.replace s d t)
         | (P.PStringReplace, _) => raise Execute "bad stringreplace"

         (* don't bother checking that the annotation matches
            the exact number of args *)
         | (P.PJointext _, args) => B.String `
             String.concat (map 
                            (fn B.String s => s 
                              | _ => raise Execute "jointext expects strings") args)

         (* XXX more primops... *)

         | (po, _) => raise Execute ("unimplemented primop " ^ P.tostring po))

    | B.Dp _ => exp
    | B.Dlookup _ => exp
    | B.Dsham { d, v } => B.Dsham { d = d, v = evaluate i G v }
    | B.Dall (sl, e) => B.Dall (sl, evaluate i G e)
    | B.Dat { d, a } => B.Dat { d = evaluate i G d,
                                a = evaluate i G a }
    | B.Drec sel => B.Drec ` ListUtil.mapsecond (evaluate i G) sel
    | B.Dsum seol => B.Dsum ` ListUtil.mapsecond (Option.map (evaluate i G)) seol
    | B.Dmu (b, sdl) => B.Dmu (b, ListUtil.mapsecond (evaluate i G) sdl)
    | B.Dexists { d, a } => B.Dexists { d = d, a = map (evaluate i G) a }

    | B.Primcall (s, el) =>
        let
          val vl = map (evaluate i G) el
        in
          case (s, vl) of
            ("display", [B.String str]) =>
               let in
                 print " ================== DISPLAY ==================\n";
                 print (str ^ "\n");
                 print " =============================================\n";
                 B.Record nil
               end
          | ("display", _) => raise Execute "wrong args to display"
          | ("version", _) => B.String Version.version
          | ("trivialdb.read", [B.String k]) => 
               let 
                 val v = TrivialDB.read k
               in
                 print ("Read " ^ k ^ " ==> " ^ v ^ "\n");
                 B.String v
               end
          | ("trivialdb.read", _) => raise Execute "wrong args to trivialdb.read"
          | ("trivialdb.update", [B.String k, B.String v]) =>
               let in
                 print ("Update " ^ k ^ " <== " ^ v ^ "\n");
                 TrivialDB.update k v;
                 B.Record nil
               end
          | ("trivialdb.update", _) => raise Execute "wrong args to trivialdb.update"
          | ("trivialdb.addhook", [B.String k, B.Record fields]) => 
               (case ListUtil.sort (ListUtil.byfirst String.compare) fields of
                  ([("d", dict), ("v0", env), ("v1", B.Record fg)]) =>
                    let
                      val (f, g) = (case fg of
                                      [("f", B.Int f), ("g", B.Int g)] => (f, g)
                                    | [("g", B.Int g), ("f", B.Int f)] => (f, g)
                                    | _ => raise Execute "trivialdb bad closure (fg)")

                      fun hook () =
                        case i of
                          I { active = ref true, threads, ... } =>
                            let in
                              threads := Q.enq ({ global = (IntConst.toInt g, 
                                                            IntConst.toInt f),
                                                  (* it's a unit cont, so give it a unit
                                                     argument too. *)
                                                  args = [env, B.Record nil] }, !threads);
                              (* always keep the hook. *)
                              true
                            end

                        (* if the instance has died, remove this hook *)
                        | I { active = ref false, ... } => false
                    in
                      TrivialDB.addhook k hook;
                      B.Record nil
                    end
                | _ => raise Execute "trivialdb needs closure")
          | _ => raise Execute ("primcall " ^ s ^ " not implemented")
        end
    | B.Var s => 
        (case SM.find (G, s) of
           NONE => raise Execute ("unbound variable " ^ s)
         | SOME e => e)


  and come (I { threads, locals, ... }) s =
    let 
      val () = print ("Come: " ^ s ^ "\n")
      (* we always expect the same type of thing,
         (exists arg . {arg cont, arg}) at server
         *)
      val entry_dict =
        B.Dexists { d = "entry",
                    a = [B.Dp B.Dcont,
                         B.Dlookup "entry"] }
    in
      case Marshal.unmarshal locals entry_dict s of
        B.Record [("d", _),
                  ("v0", B.Record [("g", B.Int g),
                                   ("f", B.Int f)]),
                  ("v1", arg)] =>
        let in
          print ("Enqueued thread " ^ IntConst.toString g ^ "." ^
                 IntConst.toString f ^ " ok\n");
          threads := Q.enq ({ global = (IntConst.toInt g, 
                                        IntConst.toInt f),
                              args = [arg] }, !threads)
        end
      | _ => raise Execute "unmarshal in come returned wrong value"
    end

  fun message (I { messages, ... }) =
    case Q.deq ` !messages of
      (NONE, _) => NONE
    | (SOME m, q) => (messages := q; SOME m)

end
