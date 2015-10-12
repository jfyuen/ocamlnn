type neuron =
{
  input : (float * float) array; (* weight, old dw *)
  mutable output : float;
  mutable activation : float;
  deltae : (float * float) array; (* gradient, old gradient *)
  update : float array; (* Update value for each weight *)
}

type algo =
  | Backprop of float * float
  | Quickprop of float * float * float * float
  | Rprop of float * float * float * float
  | SuperSAB of float * float * float * float * bool

(* input -> threshold -> output -> update *)
val create : (float * float) array -> float -> float -> float -> neuron 

val print : neuron -> unit

val apply_weights : neuron -> algo -> unit
