open Matrix

(* Get matrix from pgm *)
val get_image : string -> (int * int * float array array)

(* Write pgm *)
val write : float array array -> string -> unit

(* parse [str] : parse a pgm file called [str] *)
(* returns  64*1 matrix made from the 8*8 blocks of the pgm *)
val parse : (int * int * float array array) -> int -> float matrix list 

