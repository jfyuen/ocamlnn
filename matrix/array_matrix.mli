open Element_intf
open Matrix_intf

module ArrayMatrix : functor (Elt : ELEMENT) -> MATRIX with type elt = Elt.t

(* Some matrix types *)
module IntArrayMatrix : MATRIX with type elt = int
module FloatArrayMatrix : MATRIX with type elt = float
