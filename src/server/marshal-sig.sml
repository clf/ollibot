signature MARSHAL =
sig

  exception Marshal of string

  type locals
  val new : unit -> locals

  (* unmarshal locals dict bytes *)
  val unmarshal : locals -> Bytecode.exp -> string -> Bytecode.exp

  (* marshal locals dict value *)
  val marshal : locals -> Bytecode.exp -> Bytecode.exp -> string

end