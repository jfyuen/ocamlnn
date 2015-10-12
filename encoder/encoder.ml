open Init
open Read_conf
module M = Matrix
module B = Neural_net.Backpropagation
module BP = Neural_net.MBackpropagation

let conf_file = ref "mlp.conf"
let nn_file = ref "decoder.nn"
let img = ref "compressed_image.img"
let pgm_file = ref ""

let write_weights l =
  let oc = open_out_bin !nn_file in
  Marshal.to_channel oc l [];
  close_out oc

let write_small_img w h m =
  let oc = open_out_bin !img in
  let vals = [float_of_int w; float_of_int h] in
  let m1 = Array.to_list (Array.map Array.to_list m) in
  Marshal.to_channel oc (vals :: m1) [];
  close_out oc

let perform = function
  | "Pca" -> AcpInit.perform
  | "Gha" -> GhaInit.perform
  | "Apex" -> ApexInit.perform
  | "Ala" -> AlaInit.perform
  | "Random" -> RandomInit.perform
  | _ -> failwith "Unknown Init."

let apply () =
  let vals = read_conf !conf_file in
  print_endline "Config File successfully read.\nBegin reading image.";
  flush stdout;
  let (width, height, mat) as tmp_img = Pgm.get_image !pgm_file in
  let layers = vals.layers in
  let nb_inputs = int_of_float (sqrt (float_of_int layers.(0))) in
  let image = Pgm.parse tmp_img nb_inputs in
  let datas =
    List.map (fun x ->
      Array.of_list (List.hd (M.to_list (M.transpose x)))) image
  in
  let dataset = Array.of_list (List.map (fun x -> (x, x)) datas) in
  print_endline ("Image read, beginning " ^ vals.init ^ ".");
  flush stdout;
  let mat = (perform vals.init) image layers.(1) in
  print_endline "Init done.\nCreating Neural Net.";
  flush stdout;
  let nn_tmp = AcpInit.get_nn layers mat in
  let nn_list =
    let rand () = 
      let m = if Random.float 1. < 0.5 then -1. else 1. in
      Random.float max_float /. max_float *. m in
    Array.map (fun x -> List.map (fun y -> rand () :: y) x) nn_tmp
  in
  print_endline "Neural Net created.";
  flush stdout;
  let nn_tmp2 = B.of_list nn_list vals.vals vals.activation vals.learning in
  let nn = (
    if vals.gn then begin
      print_endline "Beginning Gauss-Newton.";
      flush stdout;
      Backprop.gauss_newton nn_tmp2 dataset
    end
    else nn_tmp2)
  in
  let b =
    if (vals.learning = "SBackprop" || vals.learning = "IBackprop") then false
    else true
  in
  let rnd = if vals.learning = "IBackprop" && not b then false else true in
  print_endline ("Learning with " ^ vals.learning ^ ".");
  flush stdout;
  let new_nn =
    BP.teach_network ~batch:b dataset vals.eps vals.max_iter
    vals.stable_eps vals.stable_iter ~rand:rnd nn
  in
  let saved_nn = B.to_list new_nn in
  print_endline ("Learning finished.\nSaving decoder to " ^ !nn_file ^".");
  write_weights saved_nn.(1);
  let small_img =
    BP.classify (Array.of_list datas) (
      B.of_list [|saved_nn.(0)|] vals.vals vals.activation vals.learning)
  in
  print_endline ("Saving compressed image to " ^ !img ^ ".");
  write_small_img width height small_img;
  flush stdout

let args =
  [
    ("-f", Arg.String (fun x -> conf_file := x),
    "File to load the config.");
    ("-d", Arg.String (fun x -> nn_file := x),
    "File to save the neural net.");
    ("-o", Arg.String (fun x -> img := x),
    "File to save the compressed image.");
  ]

let usage = Printf.sprintf "%s image.pgm" Sys.argv.(0)

let exit_usage s =
  Arg.usage args s;
  exit 1

let anon_arg =
  let pos = ref 0 in
  fun s ->
    if !pos = 0 then begin
      pgm_file := s;
      incr pos
    end
    else exit_usage (usage ^ "\n-> Too many arguments")

let () =
  if Array.length Sys.argv = 1 then exit_usage usage;
  Arg.parse args anon_arg usage;
  if !pgm_file = "" then exit_usage usage;
  apply ()

