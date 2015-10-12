module Make (Nn : Model.NEURAL_NET) = struct
  
  (* The neural net *)
  type neural_net = Nn.neural_net

  (* apply [f] to a [nn] on a ([invec],[outvec]) array. *)
  let applyset f dataset ?(rand=false) nn =
    let rec teachset_ len old_error =
      if len = 0 then old_error
      else (
        let rand = Random.int len in
        let new_len = len - 1 in
        let old = dataset.(new_len) in
        let err = f dataset.(rand) nn in
        dataset.(new_len) <- dataset.(rand);
        dataset.(rand) <- old;
        teachset_ (new_len) (err +. old_error)
      )
    in
    (* Random permutations *)
    if rand then
      begin
        Random.self_init ();
        teachset_ (Array.length dataset) 0.
      end
    else
      begin
        let err = ref 0. in
        for i = 0 to Array.length dataset - 1 do
          err := !err +. f dataset.(i) nn
        done;
        !err
      end

  (* teach a [nn] with an (invec,outvec) array. Return error *)
  let teachset = applyset Nn.teach

  (* test a [nn] with an (invec,outvec) array. Return error *)
  let testset = applyset Nn.test

  (* Iterate teaching on a neural network *)
  let teach_network ?(batch=true) dataset eps maxiter limit stable ?(rand=true) nn =
    let iter = ref 0 in
    let old_iter = ref 0 in
    let err = ref 1. in
    let cpt = ref 0 in
    let old_err = ref 0. in
    let datasize = float_of_int (Array.length dataset) in
    while !err > eps && !iter < maxiter && !cpt < stable do
      err := (
        if batch then Nn.teach_batch dataset nn
        else teachset dataset ~rand:rand nn) /. datasize;
      Printf.printf "Iter: %i, Error: %f\n" !iter !err;
      flush stdout;
      (* Stop if error is stable *)
      if abs_float (!err -. !old_err) < limit then begin
        if !old_iter = !iter - 1 then begin
          incr cpt;
          old_iter := !iter;
        end
        else begin
          cpt := 0;
          old_iter := !iter;
        end
      end;
      incr iter;
      old_err := !err;
    done;
    if !cpt >= stable then Printf.printf "Error couldn't be minimized\n";
    nn

  (* Classify values given as inputs for the neural network *)
  let classify dataset nn =
    Array.init (Array.length dataset) (fun i -> Nn.apply dataset.(i) nn)

end

(* Backpropagation *)
module Backpropagation = Backprop
module MBackpropagation = Make (Backpropagation)
