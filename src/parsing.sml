

structure Parser = struct

open Parsing
structure ST = SimpleTok

datatype token = 
    LCURLY of string
  | 
