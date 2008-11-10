signature TERMS = sig

  type index
  type matchmap
  type dbase
  type queue

  type fact
  type efact
  type sfact
  type signat

  val compile : signat -> index * queue * matchmap * dbase * efact list

  (* Removes a fact from the database; simultaneously removes
   * all dependent efacts and sfacts from the related queue and index. *)
  val use_efact : dbase * efact -> unit
  val use_sfact : dbase * sfact -> unit

  (* Seek functions *)
  datatype result = Success of unit | Failure of unit
  datatype queue_elem = EF of efact | SF of sfact
  datatype conc = EFact of efact | Conc of fact list
  val conclude : efact * sfact -> conc
                                                 
  val seek_sfact : (efact -> result) -> sfact -> result
  val seek_efact : (sfact -> result) -> efact -> result

  val insert_efact : index * efact -> unit 
  val insert_sfact : index * sfact -> unit 

  val enqueue_efact : queue * efact -> unit
  val enqueue_sfact : queue * sfact -> unit
  val dequeue : queue -> queue_elem option
 
  val matchapp : (sfact -> unit) -> matchmap * fact -> unit
  val member : dbase * fact -> bool

end
