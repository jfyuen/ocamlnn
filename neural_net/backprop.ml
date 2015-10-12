module M = Matrix
module N = Neuron

type neuron = N.neuron

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
    algo : N.algo;
    algo_name : string;
  }

(* Create neural net from an array of lists -> weights *)
let of_list nb inits f algo =
  (* Create neural net from nb + 1 (output layer added) *)
  let nc =
    Array.append [| List.length (List.hd nb.(0)) - 1|] (Array.map (fun x ->
      List.length x) nb) in
  {
    nodecount = nc;
    net = (
      let nn_size = Array.length nc in
      let layers = Array.make nn_size [||] in
      let update = if algo = "SuperSAB" then inits.(0) else inits.(8) in
      for i = 0 to nn_size - 1 do
        assert (nc.(i) > 0);
        layers.(i) <-
          Array.make nc.(i) (N.create [||] 0. 0. update);
          for j = 0 to nc.(i) - 1 do
            if i > 0 then begin (* Avoid initializing input neurons *)
              (* init weights with previous layer from nb *)
              let weights =
                List.map (fun x -> (x, 0.)) (List.nth nb.(i - 1) j) in
              let array_weights = Array.of_list (List.tl weights) in
              layers.(i).(j) <-
                N.create array_weights (fst (List.hd weights)) 0. update
            end else layers.(i).(j) <- N.create [||] 0. 0. update
          done
      done;
      layers);
    func = (function
      | "Sigmoid" -> Sigmoid
      | "QPSigmoid" -> QPSigmoid
      | _ -> failwith "Unknown Activation function\n") f;
    algo = (
      match algo with
      | "IBackprop" | "SBackprop" | "Backprop" ->
          N.Backprop (inits.(0), inits.(1))
      | "Quickprop" ->
          N.Quickprop (inits.(0), inits.(1), inits.(2), inits.(3))
      | "Rprop" -> N.Rprop (inits.(4), inits.(5), inits.(6), inits.(7));
      | "SuperSAB" ->
          N.SuperSAB (inits.(1), inits.(9), inits.(10), inits.(11), false);
      | _ -> failwith "Unknown Algorithm");
    algo_name = algo;
  }

(* Save the nn into an array of list list -> weights *)
let to_list nn =
  let list_from_neuron n =
    let inputs = Array.init (Array.length n.N.input) (fun i ->
      fst n.N.input.(i)) in
    Array.to_list inputs in
  let weights =
    Array.init (Array.length nn.nodecount - 1) (fun i ->
      Array.make nn.nodecount.(i + 1) []) in
  (* Create arrays from input weights for each neuron *)
  for i = Array.length nn.nodecount - 1 downto 1 do
    for j = 0 to nn.nodecount.(i) - 1 do
      weights.(i - 1).(j) <- list_from_neuron nn.net.(i).(j)
    done
  done;
  Array.map Array.to_list weights

(* Return the input and output layers size *)
let size nn = (nn.nodecount.(0), nn.nodecount.(Array.length nn.nodecount - 1))

(* Compute the sum of weights and inputs for a neuron *)
let get_sum node layer =
  let res = ref 0. in
  for i = 0 to Array.length node.N.input - 1 do
    if i = 0 then res := !res +. fst node.N.input.(i)
    else res := !res +. layer.(i - 1).N.output *. fst node.N.input.(i)
  done;
  !res

(* Apply activation function *)
let apply_func input = function
  | Linear -> input
  | QPSigmoid | Sigmoid -> 1. /. (1. +. exp(-.input))
  | Tanh -> 2. /. (1. +. exp (-2. *. input)) -. 1.
  | _ -> failwith "E: apply_func, not implemented yet."

(* Apply derivative functions. *)
let derivate v = function
  | Linear -> 1.
  | Sgn -> failwith "E: derivate, not implemented yet."
  | Sigmoid -> v.N.output *. (1. -. v.N.output)
  | QPSigmoid -> v.N.output *. (1. -. v.N.output) +. 0.1
  | Tanh -> 1. -. v.N.output *. v.N.output

(* Calculate the error from [tab] *)
let calc_error tab = Array.fold_left (fun x y -> x +. y *. y) 0. tab *. 0.5

(* Generate the first error array, using [outvec] for desired and [last] for
 * the last layer ouput *)
let first_error outvec last =
  Array.init (Array.length outvec) (fun i -> outvec.(i) -. last.(i))

(* given a vector [indata], apply it to a [nn] *)
let apply_gen indata nn =
  assert (Array.length indata = nn.nodecount.(0));
  for i = 0 to nn.nodecount.(0) - 1 do
    nn.net.(0).(i).N.output <- indata.(i)
  done;
  let size = Array.length nn.nodecount - 1 in
  for i = 1 to size do
    for j = 0 to nn.nodecount.(i) - 1 do
      let node = nn.net.(i).(j) in
      nn.net.(i).(j).N.activation <- get_sum node nn.net.(i - 1);
      nn.net.(i).(j).N.output <- apply_func nn.net.(i).(j).N.activation nn.func
    done
  done

(* apply [indata] to the [nn] and get the final result *)
let apply indata nn =
  apply_gen indata nn;
  let size = Array.length nn.net - 1 in
  let len = Array.length nn.net.(size) in
  Array.init len (fun n -> nn.net.(size).(n).N.output)

(* Use [expected] result to backpropagate the errors throughout the [nn] *)
let backpropagate ?(batch=false) expected nn =
  if (not batch) && (nn.algo_name <> "SBackprop" && nn.algo_name <> "IBackprop") then
    failwith (nn.algo_name ^ "does not support batch mode");
  let size = Array.length nn.net - 1 in
  let len = Array.length nn.net.(size) in
  let last = Array.init len (fun i -> nn.net.(size).(i).N.output) in
  assert (len = Array.length expected);
  let start = first_error expected last in
  (* Error to be computed for each layer *)
  let dwsums =
    Array.init (size + 1)
    (fun i -> if i = size then start else Array.make nn.nodecount.(i) 0.) in
  let err = calc_error start in
  for i = size downto 1 do
    for j = 0 to nn.nodecount.(i) - 1 do
      let node = nn.net.(i).(j) in
      let f' = derivate node nn.func in
      let delta = f' *. dwsums.(i).(j) in (* Local gradient *)
      (* Update weights *)
      for k = 0 to nn.nodecount.(i - 1) do
        let w = fst nn.net.(i).(j).N.input.(k) in
        let output = if k = 0 then 1. else nn.net.(i - 1).(k - 1).N.output in
        let current_delta = nn.net.(i).(j).N.deltae.(k) in
        let dw = delta *. output in
        nn.net.(i).(j).N.deltae.(k) <-
          (dw +. (fst current_delta), snd current_delta);
        (* Compute the deltas for the hidden layer *)
        if k > 0 then
          dwsums.(i - 1).(k - 1) <-
            dwsums.(i - 1).(k - 1) +. w *. delta;
      done
    done
  done;
  err

(* Apply weights to compute in the nn *)
let apply_weights err =
  let prev_err = ref max_float in
  fun nn ->
    for i = Array.length nn.net - 1 downto 1 do
      for j = 0 to nn.nodecount.(i) - 1 do
        let _ =
          match nn.algo with
          | N.SuperSAB (a, b, c, d, _) as sab ->
              if err > !prev_err then N.SuperSAB (a, b, c, d, true)
              else sab
          | _ -> nn.algo in
        prev_err := err;
        N.apply_weights nn.net.(i).(j) nn.algo
      done
    done

(* teach a [nn] on a ([invec], [outvec]) pair. Return error *)
let teach (invec, outvec) nn =
  if (nn.algo_name <> "SBackprop" && nn.algo_name <> "IBackprop") then
    failwith (nn.algo_name ^ "does not support batch mode");
  apply_gen invec nn;
  let err = backpropagate outvec nn in
  apply_weights err nn;
  err

(* Teach the neural network in batch mode, use a dataset *)
let teach_batch dataset nn =
  let size = Array.length dataset in
  let error = ref 0. in
  for k = 0 to size - 1 do
    apply_gen (fst dataset.(k)) nn;
    let local_err = backpropagate ~batch:true (snd dataset.(k)) nn in
    error := !error +. local_err;
  done;
  apply_weights !error nn;
  !error

(* test a [nn] an ([invec], [outvec]) pair. Return error *)
let test (invec, outvec) nn =
  let last = apply invec nn in
  let err = first_error outvec last in
  calc_error err

(* Print a [nn] *)
let print nn =
  for i = 0 to Array.length nn.net - 1 do
    for j = 0 to Array.length nn.net.(i) - 1 do
      N.print nn.net.(i).(j);
      print_newline ();
    done;
    print_newline ();
    print_newline ();
  done
  
let get_error data_size inputs outputs activ errors data snd_layer =
  let local_err = ref 0. in
  for i = 0 to data_size - 1 do
    outputs.(i) <- apply inputs.(i) snd_layer;
    for j = 0 to snd_layer.nodecount.(1) - 1 do
      activ.(i).(j) <- snd_layer.net.(1).(j).N.activation
    done;
    errors.(i) <- first_error outputs.(i) data.(i);
    local_err := !local_err +. calc_error errors.(i)
  done;
  !local_err

let gauss_newton nn dataset =
  assert (Array.length nn.nodecount = 3);
  let fst_layer =
    {
      nn with
      nodecount = [| nn.nodecount.(0); nn.nodecount.(1) |];
      net = [| nn.net.(0); nn.net.(1) |];
    } in
  let snd_layer =
    {
      nn with
      nodecount = [| nn.nodecount.(1); nn.nodecount.(2) |];
      net = [| nn.net.(1); nn.net.(2) |];
    } in
  let data_size = Array.length dataset in
  let data = Array.map snd dataset in
  let inputs = Array.make data_size [||] in
  let outputs = Array.make data_size [||] in
  let activ =
    Array.init data_size (fun i -> Array.make snd_layer.nodecount.(1) 0.) in
  let mat = Array.make_matrix data_size (snd_layer.nodecount.(0) + 1) 0. in
  let errors = Array.make data_size [||] in
  let local_err = ref 0. in
  let err = ref 0. in
  for i = 0 to data_size - 1 do
    inputs.(i) <- apply (fst dataset.(i)) fst_layer;
  done;
  err := get_error data_size inputs outputs activ errors data snd_layer;
  let node = N.create [||] 0. 0. 0. in
  let iter = ref 0 in
  while !err -. !local_err > 1e-6 do
    if !local_err <> 0. then err := !local_err;
    (* Create the J matrix *)
    for i = 0 to snd_layer.nodecount.(1) - 1 do
      for j = 0 to data_size - 1 do
        node.N.output <- outputs.(j).(i);
        node.N.activation <- activ.(j).(i);
        (* Local gradient *)
        let delta = derivate node nn.func in
        for k = 0 to snd_layer.nodecount.(0) do
          let output = if k = 0 then 1. else inputs.(j).(k - 1) in
          mat.(j).(k) <- delta *. output
        done
      done;
      let inv = M.pseudo_inverse (M.of_array mat) in
      let weights =
        M.of_array (Array.map (fun x ->
          [|fst x|]) snd_layer.net.(1).(i).N.input) in
      let current_error = Array.init data_size (fun j -> [|errors.(j).(i)|]) in
      let j_error = M.mul inv (M.of_array current_error) in
      let new_weights = M.sub weights j_error in
      for j = 0 to nn.nodecount.(1) do
        nn.net.(2).(i).N.input.(j) <- (M.get new_weights j 0, 0.)
      done
    done;
    local_err :=
      get_error data_size inputs outputs activ errors data snd_layer;
    Printf.printf "Iter: %i, Error: %f\n" !iter (!err -. !local_err);
    flush stdout;
    incr iter;
  done;
  nn
