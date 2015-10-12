module B = Neural_net.Backpropagation
module BP = Neural_net.MBackpropagation

let nn_file = ref ""
let img = ref ""
let output_img = ref ""

let get_file name : float list list =
  let ic = open_in_bin name in
  let res = Marshal.from_channel ic in
  close_in ic;
  res

let apply () =
  let vals = Array.make 8 0. in
  let dec = get_file !nn_file in
  let nn = B.of_list [|dec|] vals "Sigmoid" "Backprop" in
  let windows = List.map Array.of_list (get_file !img) in
  let dataset = Array.of_list (List.tl windows) in
  let tab = BP.classify dataset nn in
  let size = List.hd windows in
  let (w, h) = (int_of_float size.(0), int_of_float size.(1)) in
  let mat = Array.make_matrix w h 0. in
  let len = Array.length tab in
  let size = int_of_float (sqrt (float_of_int (List.length dec))) in
  let cut = w / size in
  for i = 0 to len - 1 do
    let delta_x = size * (i mod cut)
    and delta_y = size * (i / cut) in
    for j = 0 to size - 1 do
      for k = 0 to size - 1 do
        mat.(delta_y + k).(delta_x + j) <- tab.(i).(j * size + k)
      done
    done
  done;
  print_endline ("Image written to " ^ !output_img ^ ".");
  Pgm.write mat !output_img

let args =
  [
    ("-d", Arg.String (fun x -> nn_file := x),
    "File to load the neural net.");
    ("-i", Arg.String (fun x -> img := x),
    "File to load the compressed image.");
    ("-o", Arg.String (fun x -> output_img := x),
    "File to save the output image.");
  ]

let usage = Printf.sprintf "%s" Sys.argv.(0)

let exit_usage s =
  Arg.usage args s;
  exit 1

let anon_arg _ =
    exit_usage (usage ^ "\n-> No arguments needed")

let () =
  Arg.parse args anon_arg usage;
  if !nn_file = "" || !img = "" || !output_img = "" then exit_usage usage;
  apply ()

  
