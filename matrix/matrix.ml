open Element_intf
open Element

(* Keep the old array *)
module StdArray = Array

module Array = struct
  include List

  let unsafe_get = nth
  let get = nth

end

module GenMatrix (Elt : ELEMENT) = struct

  type elt = Elt.t

  type 'a matrix =
    {
      mat : 'a list list; (* Hold values within lists *)
      row : int;          (* row size *)
      col : int;          (* column size *)
    }

  exception Bad_size of string
  exception Not_inversible

  let name = "Matrix."

  (***********************************************)
  (*                  Create                     *)
  (***********************************************)

  (* Create a list of length [n] with value [def] *)
  let make_list def n =
    let rec make_list_ acc = function
      | 0 -> acc
      | n -> make_list_ (def :: acc) (n - 1)
    in
    make_list_ [] n

  (* Create a 'a list list of size row * col *)
  let make_matrix row col def =
    let line = make_list def col in
    make_list line row

  (* Replace a value with [v] within a list at pos [i] *)
  let list_replace v i l =
    let rec insert_ acc pos = function
      | [] -> []
      | h :: t when pos = i -> (List.rev (v :: acc)) @ t
      | h :: t -> insert_ (h :: acc) (pos + 1) t
    in
    insert_ [] 0 l

  (* Create the matrix *)
  let create rows cols def =
    if rows <= 0 && cols <= 0 then raise (Bad_size (name ^ "create"));
    {
      mat = make_matrix rows cols def;
      row = rows;
      col = cols;
    }

  (* Create the matrix from [l] *)
  let of_list l =
    let len = List.length (List.hd l) in
    List.iter (fun x ->
      if List.length x <> len then raise (Bad_size (name ^ "of_list")))
    l;
    {
      mat = l;
      row = List.length l;
      col = len;
    }

  (* Output the matrix into a elt list list *)
  let to_list mat = mat.mat
  
  (* Create the matrix from [l] *)
  let of_array t =
    {
      mat = StdArray.to_list (StdArray.map StdArray.to_list t);
      row = StdArray.length t;
      col = StdArray.length (StdArray.get t 0);
    }

  (* Output the matrix into a elt array array *)
  let to_array mat = StdArray.of_list (List.map StdArray.of_list mat.mat)

  (* Parse a matrix from stdin *)
  let parse_matrix () =
    let rec parse_matrix_ l =
      let s = read_line () in
      if s <> "" then
        let ls = Str.split (Str.regexp " ") s in
        parse_matrix_ (l @ [(List.map Elt.of_string ls)])
      else l
    in
    of_list (parse_matrix_ [])

  (* Copy the matrix *)
  let copy m =
    {
       mat = List.map (List.map (fun x -> x)) m.mat;
       row = m.row;
       col = m.col;
    }

  (***********************************************)
  (*              Generic Operations             *)
  (***********************************************)

  (* Matrix size *)
  let size mat = (mat.row, mat.col)

  (* Get value : row * col *)
  let get mat row col =
    if row >= mat.row || col >= mat.col || row < 0 || col < 0 then
      raise (Bad_size (name ^ "get"));
    mat.mat.(row).(col)

  (* Set a value *)
  let set mat row col v =
    if row >= mat.row || col >= mat.col || row < 0 || col < 0 then
      raise (Bad_size (name ^ "set"));
    let res = list_replace v col mat.mat.(row) in
    {
      mat = list_replace res row mat.mat;
      row = mat.row;
      col = mat.col;
    }

  (* Iter on matrix elements with [f] *)
  let iter f mat = List.iter (fun x -> List.iter f x) mat.mat

  (* Map matrix [mat] with [f] *)
  let map f mat =
    {
      mat = List.map (fun x -> List.map f x) mat.mat;
      row = mat.row;
      col = mat.col;
    }

  (* Apply f on mat1 and mat2 *)
  let rec map2 f mat1 mat2 =
    if mat1.row <> mat2.row || mat1.col <> mat2.col then
      raise (Bad_size (name ^ "map2"));
    {
      mat = List.map2 (fun x y -> List.map2 f x y) mat1.mat mat2.mat;
      row = mat1.row;
      col = mat1.col;
    }

  (* Create an identity matrix *)
  let identity size =
    let make_id_list i n =
      let rec make_id_list_ acc = function
        | 0 -> acc
        | n when n = i -> make_id_list_ (Elt.one :: acc) (n - 1)
        | n -> make_id_list_ (Elt.zero :: acc) (n - 1)
      in
      make_id_list_ [] n
    in
    {
      mat = (
        let rec make_id acc = function
          | 0 -> acc
          | n -> make_id (make_id_list n size :: acc) (n - 1)
        in
        make_id [] size);
      row = size;
      col = size;
    }

  (* Compute the trace *)
  let trace m =
    if m.row <> m.col then raise (Bad_size (name ^ "trace"));
    let rec trace_ accu = function
      | 0 -> accu
      | n -> trace_ (Elt.add accu m.mat.(n).(n)) (n - 1)
    in
    trace_ Elt.zero (m.row - 1)

  (* Concat 2 matrix *)
  let concat m1 m2 =
    if m1.row <> m2.row then raise (Bad_size (name ^ "concat"));
    let rec add_to_queue l1 l2 =
      match (l1, l2) with
      | ([], []) -> []
      | (h1 :: t1, h2 :: t2) -> (h1 @ h2) :: add_to_queue t1 t2
      | _ -> assert false
    in
    {
      mat = add_to_queue m1.mat m2.mat;
      row = m1.row;
      col = m1.col + m2.col;
    }

  (* Create a random matrix *)
  let random row col lim =
    let mat = create row col lim in
    Random.self_init ();
    map Elt.random mat

  (* Transpose the matrix *)
  let transpose mat =
    let rec get_line acc acc2 = function
      | []  -> (List.rev acc, List.rev acc2)
      | [] :: t -> get_line acc acc2 t
      | (h1 :: []) :: t -> get_line (h1 :: acc) acc2 t
      | (h1 :: t1) :: t -> get_line (h1 :: acc) (t1 :: acc2) t
    in
    let rec app acc = function
      | [] -> List.rev acc
      | m ->
          let (l, tmpmat) = get_line [] [] m in
          app (l :: acc) tmpmat
    in
    {
      mat = app [] mat.mat;
      row = mat.col;
      col = mat.row;
    }

  (* Print the matrix *)
  let print mat =
    List.iter (fun x ->
      List.iter (fun y ->
        Elt.print y; print_string "\t")
      x; print_newline ())
    mat.mat


  (***********************************************)
  (*            Op on Elements                   *)
  (***********************************************)

  (* Basic operations on a matrix and an element *)
  let apply_elem f mat elem = map (fun x -> f x elem) mat

  let add_elem = apply_elem Elt.add
  let sub_elem = apply_elem Elt.sub
  let mul_elem = apply_elem Elt.mul
  let div_elem = apply_elem Elt.div

  (* Basic operations on 2 matrix *)
  let add = map2 Elt.add

  let sub = map2 Elt.sub

  let mul mat1 mat2 =
    if mat1.col <> mat2.row then raise (Bad_size (name ^ "mul"));
    let compute_elem l1 l2 =
      let l = List.map2 (fun x y -> Elt.mul x y) l1 l2 in
      List.fold_left Elt.add Elt.zero l
    in
    let compute_line l1 l2 = List.map (compute_elem l1) l2 in
    let res =
      let trans = transpose mat2 in
      List.map (fun x -> compute_line x trans.mat) mat1.mat
    in
    {
      mat = res;
      row = mat1.row;
      col = mat2.col;
    }


  (* Gauss method *)
  let gauss m =
    if m.row <> m.col then raise (Bad_size (name ^ "inverse"));
    (* concat id to the end of current matrix *)
    let addid m =
      let id = identity m.row in
      concat m id
    in
    (* build the line where the pivot is *)
    let rec build_pivotline l k piv=
      match l with
      | [] -> []
      | a :: q ->
          begin
            match k with
            | 1 ->
                if (a <> Elt.zero) then Elt.one :: (build_pivotline q (k-1) a)
                else raise Not_inversible
            | n when n > 0 -> a :: build_pivotline q (k-1) piv
            | n -> Elt.div a piv :: build_pivotline q (k-1) piv
          end
    in
    (* Build another line *)
    let build_otherline l pivl xkj =
      List.map2 (fun a b -> Elt.sub a (Elt.mul b xkj)) l pivl
    in
    (* build the k-ieme matrix *)
    let build_kmat m k =
      let tm = transpose m in
      let pivl = build_pivotline m.mat.(k - 1) k Elt.zero in
      let pivc = tm.mat.(k - 1) in
      let rec buildm m pivl pivc k nb =
	match m with
        | [] -> []
	| a :: q ->
            if (nb = k) then pivl :: buildm q pivl (List.tl pivc) k (nb+1)
            else (
              build_otherline a pivl (List.hd pivc) ::
                buildm q pivl (List.tl pivc) k (nb + 1))
      in
      buildm m.mat pivl pivc k 1
    in
    (* enleve les k dernieres col de la matrice *)
    let rec remk m k =
      match m with
      | [] -> []
      | a::q -> if (k > 1) then remk q (k - 1) else q
    in
    let rec princ_boucle m k max =
      if (k > max) then m
      else
        let newm =
          {
            mat = build_kmat m k;
            row = m.row;
            col = m.col;
          }
    in
    princ_boucle newm (k + 1) max
    in
    let fintm = transpose (princ_boucle (addid m) 1 m.row) in
    let res =
      {
        mat = remk fintm.mat m.row;
        row = m.row;
        col = m.col;
      }
    in
    transpose res

  (* Invert a matrix *)
  let inverse = gauss

  (* Penrose pseudo inverse *)
  let pseudo_inverse m =
    let trans_mat = transpose m in
    let tmp_mat = mul trans_mat m in
    mul (inverse tmp_mat) trans_mat


end

(******************************)
(* Define some useful modules *)
(******************************)

module IntMatrix = GenMatrix (Int)
module FloatMatrix = GenMatrix (Float)
include Array_matrix.FloatArrayMatrix
