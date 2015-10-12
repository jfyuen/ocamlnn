module Fam = Matrix;;

let sep = "************************\n";;

module Gha_neuron =
struct
type neuron =
  {
    input : float Fam.matrix; 
    output : float;
    last_output : float;
    err_sum : float;
  }

let create input out =
   {
     input = input;
     output = out;
     last_output = out;
     err_sum = 0.;
   }

let display neurone = 
  print_string sep;
  Fam.iter (fun x -> print_float x; print_newline()) neurone.input;
  print_string sep

  
let forward neuron input =
   let res = Fam.mul (Fam.transpose (neuron.input)) (input) in
  { neuron with output = Fam.get res 0 0; }
end

type neuron = Gha_neuron.neuron

type layer = neuron array

type neural_net =
    {
      size : int;
      nb_input : int; 
      output_layer : layer;
    }
;;

(* Type of neural network *)
let version = "GHA"
;;


(* create a GHA network *)
let create nb nb_vect = 
  let nb_neuron = nb_vect in
  let ol = Array.make nb_neuron (Gha_neuron.create (Fam.create nb 1 0.) 0.) in 
  let  init_neuron elt = 
       Gha_neuron.create (Fam.map (fun x -> (Random.float 1.) -. 0.5) 
       elt.Gha_neuron.input) 0.
  in
  {
    size=nb_neuron;
    output_layer = Array.map init_neuron ol;
    nb_input = nb;
  }
;;

(* compute)
let compute_error nn =
  (Array.fold_left (fun x y -> 
    let err = Gha_neuron.err y in
    x +. err ** 2.) 0. nn.output_layer) 
/. (float_of_int nn.size)
;;*)

let epsilon = 0.001;;

let compute_err new_input input = 
  let res = Fam.sub new_input input in 
    Misc.norme res 
    
let adjust_weight nn input reset = 
  for j = 0 to nn.size - 1 do
    let neuron = nn.output_layer.(j) in
    let sum = ref (Fam.create (nn.nb_input) 1 0.) in
    for i = 0 to j do
      let y = nn.output_layer.(i).Gha_neuron.output in 
      let c = nn.output_layer.(i).Gha_neuron.input in 
      sum := Fam.add !sum (Fam.mul_elem c y)
    done;
   let xt = Fam.sub input !sum in
    let ct = Fam.mul_elem xt 
                          (epsilon *. nn.output_layer.(j).Gha_neuron.output) in 
  
    let new_input = Fam.add neuron.Gha_neuron.input ct in
     nn.output_layer.(j) <- 
	{
	  Gha_neuron.input = new_input;
          Gha_neuron.output = neuron.Gha_neuron.output;
          Gha_neuron.last_output = neuron.Gha_neuron.last_output;
          Gha_neuron.err_sum = 
          let err = (compute_err new_input neuron.Gha_neuron.input) in
          if (reset) then 
            err 
          else 
            neuron.Gha_neuron.err_sum +. err;
	}
  done
 (*let neuron = nn.output_layer.(0) in 
 let c = neuron.Gha_neuron.input in 
 let y2ct = Fam.mul_elem c 
(neuron.Gha_neuron.output *. neuron.Gha_neuron.output)  in 
 let yx = Fam.mul_elem input (neuron.Gha_neuron.output) in 
 let toadd = Fam.mul_elem (Fam.sub yx y2ct) epsilon in 
  nn.output_layer.(0) <- 
	{
	  Gha_neuron.input = Fam.add neuron.Gha_neuron.input toadd;
          Gha_neuron.output = neuron.Gha_neuron.output;
          Gha_neuron.last_output = neuron.Gha_neuron.last_output;
	}
 *)
 
let display nn = 
  for i = 0 to nn.size - 1 do
    Gha_neuron.display nn.output_layer.(i)
  done

let forward nn input = 
 for i = 0 to nn.size - 1 do
   nn.output_layer.(i) <- Gha_neuron.forward nn.output_layer.(i) input
  done

let get_vp nn =
   Array.fold_right (fun y x -> y.Gha_neuron.input::x)  nn.output_layer []


let get_res nn  = 
  (let res = get_vp nn in    
  (*List.iter (fun x -> Fam.print x; print_newline()) res;
       prerr_endline "done";*)
       res)

let retrieve_err nn = 
  (Array.fold_left (fun x y -> y.Gha_neuron.err_sum +. x) 0. nn.output_layer)
  /. (float_of_int nn.size)


  
let perform input nb_vect =
  (* XXX *)
 let last_err = ref 1. in 
  let size = List.length input in
  let nn = create (fst(Fam.size (List.hd input))) nb_vect in
  let rec perform_ data cpt =
    let input = List.nth data (cpt mod size) in
    forward nn input;
    let err =  (retrieve_err nn) /. (float_of_int size) in 
    let reset = if (cpt mod size = 0) then 
      true
    else 
      false in
    adjust_weight nn input reset;
     if (cpt mod size = 0 && cpt <> 0) then
     (
      print_int (cpt / size); print_string ": ";
      print_float err; print_newline();
      if (err > epsilon_float && cpt < 300000
      && (abs_float (!last_err -. err) > epsilon_float)) then 
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
  let centered = (Misc.center_it input) in
  perform_ (centered) 0
;;

