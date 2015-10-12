(* Integers *)
module Int = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div = (/)
  let abs = abs
  let eps = 0
  let print = print_int
  let random = Random.int
  let of_string = int_of_string
end

(* Floats *)
module Float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = (+.)
  let sub = (-.)
  let mul = ( *. )
  let div = (/.)
  let abs = abs_float
  let eps = 1e-20
  let print = print_float
  let random = Random.float
  let of_string = float_of_string
end

