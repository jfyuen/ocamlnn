(* Define some element *)
module type ELEMENT = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val abs : t -> t
  val eps : t
  val print : t -> unit
  val random : t -> t
  val of_string : string -> t
end

