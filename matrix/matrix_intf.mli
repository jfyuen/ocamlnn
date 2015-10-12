(* Define the matrix signature *)
module type MATRIX = sig

  (* The element *)
  type elt
  (* The matrix *)
  type 'a matrix

  exception Bad_size of string
  exception Not_inversible

  (* Create a simple matrix *)
  val create : int -> int -> elt -> elt matrix

  (* Create the matrix from [l] *)
  val of_list : elt list list -> elt matrix

  (* Output the matrix into a elt list list *)
  val to_list : elt matrix -> elt list list

  (* Create the matrix from [|l|] *)
  val of_array : elt array array -> elt matrix

  (* Output the matrix into a elt list list *)
  val to_array : elt matrix -> elt array array

  (* Copy the matrix *)
  val copy : elt matrix -> elt matrix

  (* Parse a matrix from stdin *)
  val parse_matrix : unit -> elt matrix

  (* Matrix size : row * col *)
  val size : elt matrix -> (int * int)

  (* Get value : row * col *)
  val get : elt matrix -> int -> int -> elt

  (* Set a value *)
  val set : elt matrix -> int -> int -> elt -> elt matrix

  (* Iter on matrix elements with [f] *)
  val iter : (elt -> unit) -> elt matrix -> unit

  (* Map matrix [mat] with [f] *)
  val map : (elt -> 'a) -> elt matrix -> 'a matrix

  (* Apply [f] on 2 matrix *)
  val map2 : (elt -> 'a -> 'b) -> elt matrix -> 'a matrix -> 'b matrix

  (* Create an identity matrix *)
  val identity : int -> elt matrix

  (* Compute the trace *)
  val trace : elt matrix -> elt

  (* Concat 2 matrix *)
  val concat : elt matrix -> elt matrix -> elt matrix

  (* Create a random matrix *)
  val random : int -> int -> elt -> elt matrix

  (* Transpose the matrix *)
  val transpose : elt matrix -> elt matrix

  (* Print the matrix *)
  val print : elt matrix -> unit

  (* Basic operations on a matrix and an element *)
  val add_elem : elt matrix -> elt -> elt matrix
  val sub_elem : elt matrix -> elt -> elt matrix
  val mul_elem : elt matrix -> elt -> elt matrix
  val div_elem : elt matrix -> elt -> elt matrix

  (* Basic operations on 2 matrix *)
  val add : elt matrix -> elt matrix -> elt matrix
  val sub : elt matrix -> elt matrix -> elt matrix
  val mul : elt matrix -> elt matrix -> elt matrix

  (* Invert a matrix *)
  val inverse : elt matrix -> elt matrix

  (* Penrose pseudo inverse *)
  val pseudo_inverse : elt matrix -> elt matrix

end

