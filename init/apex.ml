module Fam = Matrix;;

(* separator DEBUG*)
let sep = "************************\n";;

module APEX_neuron =
struct
type neuron =
  {
    input : float Fam.matrix; 
    output : float;
    last_output : float ;
    err_sum : float;
}


(* create [input] [out] : 
 * create a new neuron with [input] for input weights 
 * and [out] for default output value (usualy 0)*)
let create input out =
   {
     input = input;
     output = out;
     last_output = 0.;
     err_sum = 0.;
   }


(* display a neuron DEBUG *)
let display neurone = 
  print_string sep;
  Fam.iter (fun x -> print_float x; print_newline()) neurone.input;
  print_string sep;
  print_string "output:"; print_float neurone.output;
  print_newline ();
;;

  
end

type neuron = APEX_neuron.neuron

type layer = neuron array

type neural_net =
    {
      size : int;
      neuron_layer : layer;
      lateral_weight : float Fam.matrix array;
    }
;;

(* Type of neural network *)
let version = "APEX"
;;



(* compute_j [nb] : compute a lateral weight matrix for nb neurons
 * must be a inf/diag matrix *)
let compute_j nb = 
   let res = Array.make (nb) (Fam.create 1 1 0.) in
   for i = 1 to (nb - 1) do 
     res.(i) <- Fam.random i 1 1.
   done;
   res
;;
(*let compute_j nn =
   let lw = ref nn.lateral_weight in 
     for i = 0 to nn.size - 1 do 
      let ct = Fam.transpose nn.neuron_layer.(i).APEX_neuron.input in
      for j = 0 to i - 1 do 
	let cj = nn.neuron_layer.(j).APEX_neuron.input in  
        let res = (Fam.mul ct cj) in 
	lw := Fam.set !lw i j (Fam.get res 0 0 );
      done
     done;
     !lw
;;*)   

(* create [nb] : create a APEX network, the input must be a vector with 
 * length == [nb] *)
let create nb nb_vect =
    let nb_neuron = nb_vect in
    let  nl = Array.make nb_neuron (APEX_neuron.create (Fam.create nb 1 0.) 0.) in
    let  init_neuron elt = 
       APEX_neuron.create (Fam.map (fun x -> (Random.float 1.) -. 0.5) 
       elt.APEX_neuron.input) 0.
    in
    let neurons = Array.map init_neuron nl
    in
    {	
      size = nb_neuron;
      neuron_layer = neurons;
      lateral_weight = compute_j nb_neuron;
    }	
;;


(* compute_error [nn] : compute last error for the network [nn] 
let compute_error nn =
   (* let sum = ref 0. in 
    for i = 0 to nn.size - 1 do 
      for j = 0 to nn.size - 1 do 
      sum := !sum +. abs_float((Fam.get nn.lateral_weight i j))
      done 
    done;
   !sum*)
    (Array.fold_left (fun x y -> 
    let err = APEX_neuron.err y in
    x +. abs_float (err)) 0. nn.neuron_layer)
/. (float_of_int nn.size)

   
;;*)


(* display [nn] : display network [nn] *)
let display nn = 
   for i = 0 to nn.size - 1 do
    APEX_neuron.display nn.neuron_layer.(i)
   done;
   print_string "lateral weight\n";
   Array.iter (fun x -> Fam.print x; print_newline()) nn.lateral_weight;
   print_newline()
;;

(* compute_a [nn] [j] : get a lateral weight vector 
 * from network [nn] for the neuron [j]*)
let compute_a nn j = 
  nn.lateral_weight.(j)
;;

(* compute_y [nn] [j] : make a vector from the output of 
 * the [j]th first neurons, in the network [nn] *)
let compute_y nn j = 
  if (j != 0) then 
    (
      let res = ref (Fam.create j 1 0.) in
      for i = 0 to (j - 1) do 
        res := Fam.set !res i 0 (nn.neuron_layer.(i).APEX_neuron.output)
      done;
      !res 
    )
  else
    Fam.create 1 1 0.
  
;;

(* forward [nn] [input] : propagate data [input] through the network [nn]*)
let forward nn input =
  for j = 0 to nn.size - 1 do
    let neuron = nn.neuron_layer.(j) in
    let wx = Fam.mul (Fam.transpose neuron.APEX_neuron.input) input in 
    let a = compute_a nn j and
     y = compute_y nn j in
    let ay = Fam.mul (Fam.transpose a) y in 
    nn.neuron_layer.(j) <- 
      {
        APEX_neuron.input = neuron.APEX_neuron.input;
        APEX_neuron.output = Fam.get (Fam.sub wx ay) 0 0;
        APEX_neuron.last_output = neuron.APEX_neuron.output;
        APEX_neuron.err_sum = neuron.APEX_neuron.err_sum;
      }

  done
;;


let epsilon = 0.001
;;

let compute_err new_input input = 
  let res = Fam.sub new_input input in 
    Misc.norme res 
;;


let adjust_weight nn input reset =
  (* for each neuron in the layer *)
  for j = 0 to nn.size - 1 do 
    let neuron = nn.neuron_layer.(j) in 
    (* adjust weight *)
    let yx = Fam.mul_elem input neuron.APEX_neuron.output and 
    y2w = Fam.mul_elem neuron.APEX_neuron.input (neuron.APEX_neuron.output ** 2.)
    in
    let res = Fam.mul_elem (Fam.sub yx y2w) epsilon in
    let new_input = Fam.add neuron.APEX_neuron.input res in
    nn.neuron_layer.(j) <- 
      {
        APEX_neuron.input = new_input;
        APEX_neuron.output = neuron.APEX_neuron.output;
        APEX_neuron.last_output = neuron.APEX_neuron.last_output;
        APEX_neuron.err_sum = 
          let err = (compute_err new_input neuron.APEX_neuron.input) in
          if (reset) then 
            err 
          else 
            neuron.APEX_neuron.err_sum +. err;
      };
    (* adjust associated lateral weight *)
    let aj = compute_a nn j and 
    yj = compute_y nn j in
    let yyj = Fam.mul_elem yj neuron.APEX_neuron.output and
    y2a = Fam.mul_elem aj (neuron.APEX_neuron.output ** 2.) in 
    let tosub = Fam.mul_elem (Fam.sub yyj y2a) epsilon in 
     nn.lateral_weight.(j) <- Fam.add  nn.lateral_weight.(j) tosub
  done 
;;

let get_vp nn =
   Array.fold_right (fun  y x -> y.APEX_neuron.input::x)  nn.neuron_layer []

let get_res nn  = 
  (let res = get_vp nn in    
   (*List.iter (fun x -> Fam.print x; print_newline()) res;
       prerr_endline "done";*)
       res)

let retrieve_err nn = 
  (Array.fold_left (fun x y -> y.APEX_neuron.err_sum +. x) 0. nn.neuron_layer)
  /. (float_of_int nn.size)

let perform input nb_vect =
  let last_err = ref 3000. in 
  let size = List.length input in 
  (* XXX *)
  let nn = create (fst(Fam.size (List.hd input))) nb_vect  in
  let rec perform_ data cpt  =  
(*    display nn;*)
    let input = List.nth data (cpt mod size) in
    forward nn input;
    let err = (retrieve_err nn) /. (float_of_int size) in 
    let reset = if (cpt mod size = 0) then 
      true
    else 
      false in
    adjust_weight nn input reset;
    if (cpt mod size = 0 && cpt <> 0) then
     (
      print_int (cpt / size); print_string ": ";
      print_float err; print_newline();
      if (err > 1e-10
      && cpt < 600000
      && (abs_float(!last_err -. err) > 1e-12)) then 
        (
          last_err := err; 
          perform_ data (cpt + 1)
        )
      else 
         get_res nn
     )
     else 
        perform_ data (cpt + 1)

  in
  (* XXX *)
  perform_  (Misc.center_it input) 0 
    
;;
