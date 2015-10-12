module Fam = Matrix;;

let sep = "************************\n";;

module Ala_neuron =
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
   let res = Fam.mul (Fam.transpose neuron.input) input
  in
  {
       input = neuron.input; 
       output = Fam.get res 0 0;
       last_output = neuron.output;
       err_sum = neuron.err_sum;
  }
end

type neuron = Ala_neuron.neuron

type layer = neuron array

type neural_net =
    {
      size : int;
      nb_input : int; 
      output_layer : layer;
    }
;;

(* Type of neural network *)
let version = "ALA"
;;


(* create a Ala network *)
let create nb nb_vect = 
  let nb_neuron = nb_vect in
  let ol = Array.make nb_neuron (Ala_neuron.create (Fam.create nb 1 0.) 0.) in 
  let  init_neuron elt = 
       Ala_neuron.create (Fam.map (fun x -> (Random.float 1.) -. 0.5) 
       elt.Ala_neuron.input) 0.
  in
  {
    size=nb_neuron;
    output_layer = Array.map init_neuron ol;
    nb_input = nb;
  }
;;

(* compute 
let compute_error nn =
  (Array.fold_left (fun x y -> 
    let err = Ala_neuron.err y in
    x +. err ** 2.) 0. nn.output_layer)
/. (float_of_int nn.size)
;;*)

let beta = (sqrt(2.) -. 1.)
;;

(* norme [v] : [v] is a vector, compute its norm *)
let norme v = 
   let rec norme_ cpt accu =  
      let (row, col) = Fam.size v in
	match cpt with 
	n when n = row -> accu
	| _ -> 
	(
	 let x = Fam.get v cpt 0 in
	   norme_ (cpt + 1) (accu +. (x *. x)) 
	)
in
   sqrt(norme_ 0 0.)
;;

let epsilon = 0.1;;

let compute_err new_input input = 
  let res = Fam.sub new_input input in 
    Misc.norme res 

let compute_c c = 
  let n = norme c in 
   if (n *. n) > (1. /. beta) +. 0.5 then
     Fam.mul_elem c (1. /. (sqrt(2.) *. n))
   else
     c 

let adjust_weight nn input reset = 
  for j = 0 to nn.size - 1 do
    let neuron = nn.output_layer.(j) in
    let sum = ref (Fam.create (nn.nb_input) 1 0.) in
    for i = 0 to j do
      let y = nn.output_layer.(i).Ala_neuron.output in 
      let c = nn.output_layer.(i).Ala_neuron.input in 
      sum := Fam.add !sum (Fam.mul_elem c y)
    done;
    let xt = Fam.sub input !sum in 
    let ct = Fam.mul_elem xt 
                          (epsilon *. nn.output_layer.(j).Ala_neuron.output) 
                           
    in
    let new_input = compute_c (Fam.add neuron.Ala_neuron.input ct) in
    nn.output_layer.(j) <- 
	{
	  Ala_neuron.input = new_input;
          Ala_neuron.output = neuron.Ala_neuron.output;
          Ala_neuron.last_output = neuron.Ala_neuron.last_output;
          Ala_neuron.err_sum = 
          let err = (compute_err new_input neuron.Ala_neuron.input) in
          if (reset) then 
            err 
          else 
            neuron.Ala_neuron.err_sum +. err;

        }
  done
 
let display nn = 
  for i = 0 to nn.size - 1 do
    Ala_neuron.display nn.output_layer.(i)
  done

let forward nn input = 
 for i = 0 to nn.size - 1 do
   nn.output_layer.(i) <- Ala_neuron.forward nn.output_layer.(i) input
  done

let get_vp nn =
   Array.fold_right (fun y x -> y.Ala_neuron.input::x)  nn.output_layer []

let get_res nn  = 
  (let res = get_vp nn in    
   (*List.iter (fun x -> Fam.print x; print_newline()) res;
       prerr_endline "done";*)
       res)

let retrieve_err nn = 
  (Array.fold_left (fun x y -> y.Ala_neuron.err_sum +. x) 0. nn.output_layer)
  /. (float_of_int nn.size)

let perform input nb_vect =
  let eps = 1e-10 in
  let size = List.length input in
  let last_err = ref 1. in
  (* XXX *)
  let nn = create (fst(Fam.size (List.hd input))) nb_vect in
  let rec perform_ data cpt =    
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
      if (err > eps 
      && cpt < 300000
      && (abs_float (!last_err -. err) > eps)) then 
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
  perform_ (Misc.center_it input) 0
    
;;

(*
  
*)
