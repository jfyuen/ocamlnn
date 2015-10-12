type neuron

type layer = neuron array

(* Activation Function types *)
type kind =
  | Linear
  | Sgn
  | Sigmoid
  | QPSigmoid
  | Tanh

(* A neural net *)
type neural_net =
  {
    nodecount : int array;
    net : layer array;
    func : kind;
    algo : Neuron.algo;
    algo_name : string;
  }

(* Create neural net from an array of lists -> weights *)
val of_list : float list list array -> float array ->
  string -> string -> neural_net

(* Save the nn into an array of list list -> weights *)
val to_list : neural_net -> float list list array

(* Return the input and output layers size *)
val size : neural_net -> (int * int)

(* Apply weights to compute in the nn *)
val apply_weights : float -> neural_net -> unit

(* Use [expected] result to backpropagate the errors throughout the [nn] *)
val backpropagate :
  ?batch:bool -> float array -> neural_net -> float

(* given a vector indata, apply it to a net and produce
 * a vector the results for all layers *)
val apply : float array -> neural_net -> float array

(* teach a network an (invec, outvec) pair. Return error *)
val teach : (float array * float array) -> neural_net -> float

(* Teach the neural network in batch mode, use a dataset *)
val teach_batch : (float array * float array) array -> neural_net -> float

(* test a network an (invec, outvec) pair. Return error *)
val test : (float array * float array) -> neural_net -> float

val print : neural_net -> unit

val gauss_newton : neural_net -> (float array * float array) array -> neural_net
