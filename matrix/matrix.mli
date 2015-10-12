open Element_intf
open Matrix_intf

(* The functor for MATRIX *)
module GenMatrix : functor (Elt : ELEMENT) -> MATRIX with type elt = Elt.t

(* Some matrix types *)
module IntMatrix : MATRIX with type elt = int
module FloatMatrix : MATRIX with type elt = float

include MATRIX with type elt = float
