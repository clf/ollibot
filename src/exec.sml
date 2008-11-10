
functor Exec(T : TERMS) = struct

  open T

  structure PQ = 
  HeapFn(struct 
         type priority = int
         fun compare (x,y) = 
             if x > y then LESS else if y > x then GREATER else EQUAL
         end)
  

  fun execute(sgn, facts) = 
      let 
        val (ndx, queue, match, dbase, inputs) = compile sgn

        fun assert_fact (fact) = 
            if not (member(dbase, fact)) 
            then matchapp (fn sfact => enqueue_sfact(queue, sfact)) 
                          (match, fact) 
            else ()

        fun assert_facts facts = List.app assert_fact facts

        fun search (EFact efact) = 
            let fun seeker sfact = 
                    case search(conclude(efact,sfact)) of
                      Failure() => Failure()
                    | Success() => Success(use_sfact(dbase,sfact))
            in 
              case seek_efact seeker efact of 
                Failure() => Failure(insert_efact(ndx,efact))
              | Success() => Success()
            end
          | search (Conc facts) = Success(assert_facts facts)

        fun loop NONE = ()
          | loop (SOME(EF efact)) = 
            let in
              case search (EFact efact) of
                Failure() => Failure(insert_efact(ndx, efact))
              | Success() =>
                Success(use_efact(dbase, efact); 
                        enqueue_efact(queue, efact));
              loop (dequeue queue)
            end
          | loop (SOME(SF sfact)) = 
            let fun seeker efact =
                    case search(conclude(efact,sfact)) of
                      Failure() => Failure()
                    | Success() => 
                      Success(use_efact(dbase, efact);
                              use_sfact(dbase, sfact))
            in
              case seek_sfact seeker sfact of
                Failure() => Failure()
              | Success() => Success(use_sfact(dbase, sfact));
              insert_sfact(ndx, sfact);
              loop (dequeue queue)
            end
            
      in
        assert_facts facts;
        loop (dequeue queue)
      end  

end
