module type NEURAL_NET = sig
  type neuron
  type layer
  type neural_net

  (* Create neural net from an array of lists -> weights *)
  val of_list : float list list array -> float array ->
    string -> string -> neural_net

  (* Save the nn into an array of list list -> weights *)
  val to_list : neural_net -> float list list array

  (* Return the input and output layers size *)
  val size : neural_net -> (int * int)

  (* given a vector indata, apply it to a net and produce
   * a vector the results for all layers *)
  val apply : float array -> neural_net -> float array

  (* teach a network an (invec, outvec) pair. Return error *)
  val teach : (float array * float array) -> neural_net -> float

  (* Teach the neural network in batch mode, use a dataset *)
  val teach_batch : (float array * float array) array -> neural_net -> float

  (* test a network an (invec, outvec) pair. Return error *)
  val test : (float array * float array) -> neural_net -> float

end

module type MODEL = sig
  type neural_net

  (* teach a network on a (invec, outvec) array. Return average error *)
  val teachset :
    (float array * float array) array -> ?rand:bool -> neural_net -> float

  (* test a network on a (invec, outvec) array. Return average error *)
  val testset :
    (float array * float array) array -> ?rand:bool -> neural_net -> float

  (* Iterate/Batch teaching on a neural network *)
  val teach_network :
    ?batch:bool -> (float array * float array) array -> float -> int ->
      float -> int -> ?rand:bool -> neural_net -> neural_net

  (* Classify values given as inputs for the neural network *)
  val classify : float array array -> neural_net -> float array array

end
