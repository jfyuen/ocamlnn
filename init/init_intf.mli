open Matrix
module type ALGO = sig
  val version : string
  
  val perform : float matrix list -> int -> float matrix list 
end 

module type INIT = sig
  val get_nn : int array -> float matrix list -> float list list array
  include ALGO
end

