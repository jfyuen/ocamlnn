module FM = Matrix
let file_to_list name =
  let ic = open_in name in
  let rec readlines lines =
    let maybe_line =
      try
        let ln = input_line ic in
        Some ln
      with End_of_file -> None
    in
    match maybe_line with
    | Some line -> readlines (line :: lines)
    | None ->
        begin
          close_in ic;
          List.rev (List.filter (fun x -> x.[0] <> '#') lines)
        end
  in readlines []

(* Create a (width, height, int array array) from a pgm ascii file *)
let get_image name =
  let l = file_to_list name in
  if List.hd l <> "P2" then failwith "PGM: must use ascii encoding.";
  try
    let sep = Str.regexp " " in
    let size = Str.split sep (List.nth l 1) in
    let width = int_of_string (List.hd size) in
    let height = int_of_string (List.hd (List.tl size)) in
    let max_color = int_of_string (List.nth l 2) in
    let l = List.tl (List.tl (List.tl l)) in
    List.iter (fun x ->
      if int_of_string x > max_color then failwith "PGM: Color too high.")
    l;
    let tab = Array.of_list l in
    let tab_size = Array.length tab in
    if width * height <> tab_size then begin
      Printf.printf "Width: %i, height: %i, size: %i\n" width height tab_size;
      failwith "PGM: wrong size.\n"
    end;
    let mat = Array.make_matrix height width 0. in
    let color = float_of_int max_color in
    for idx = 0 to height - 1 do
      for jdx = 0 to width - 1 do
        mat.(idx).(jdx) <-
          float_of_string tab.(idx * width + jdx) /. color
      done
    done;
    (width, height, mat)
  with _ -> failwith "PGM: wrong file encoding."

(* Write to mat pgm file, order is reverse because of get_image *)
let write mat name =
  let width = Array.length mat - 1 in
  let height = Array.length mat.(0) - 1 in
  let oc = open_out name in
  output_string oc "P2\n# Created by Encoder\n";
  output_string oc (string_of_int (width + 1) ^
  " "^ string_of_int (height + 1) ^ "\n");
  output_string oc "255\n";
  for i = 0 to width do
    for j = 0 to height do
      output_string oc (
        string_of_int (int_of_float (
          mat.(width - i).(height - j) *. 255.)) ^ "\n")
    done
  done;
  close_out oc

(* Cut the image in 8x8 blocks and vectorize them *)
(* [tab] = output of the pgm parser *)
(* return a matrix list *)
let make_blocks (col, row, tab) size =
  let lim = size - 1 in
  let len = size * size in
  let vect = Array.make len [] in
  let make_list i =
    let delta_x = size * (i mod (col / size))
    and delta_y = size * (i / (col / size)) in
    let rec make_list_ x y pos =
      match (x, y) with
      | (n, m) when n = m && n = lim ->
          vect.(pos) <- [tab.(delta_y + lim).(delta_x + lim)];
          Array.to_list vect
      | (n, e) when n = lim -> 
          vect.(pos) <- [tab.(delta_y + lim).(delta_x + e)];
          make_list_ 0 (e + 1) (pos - 1)
      | (u, v) ->
          vect.(pos) <- [tab.(delta_y + u).(delta_x + v)];
          make_list_ (u + 1) v (pos - 1)
    in
    make_list_ 0 0 (len - 1)
  in
  let rec make_blocks_ i accu =
    match i with
    | n when n = (col / size) * (row / size) -> accu
    | _ -> make_blocks_ (i + 1) (FM.of_list (make_list i)::accu)
  in
  make_blocks_ 0 []
;;

let parse x size = make_blocks x size
;;
