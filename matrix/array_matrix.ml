open Element_intf
open Element

module ArrayMatrix (Elt : ELEMENT) = struct

  type elt = Elt.t

  type 'a matrix =
    {
      mat : 'a array array; (* Hold values within arrays *)
      row : int;            (* row size *)
      col : int;            (* column size *)
    }

  exception Bad_size of string
  exception Not_inversible

  let name = "Array_matrix."

  (* Create the matrix *)
  let create row col def =
    if row <= 0 && col <= 0 then raise (Bad_size (name ^ "create"));
    {
      mat = Array.make_matrix row col def;
      row = row;
      col = col;
    }

  (* Create the matrix from [l] *)
  let of_list l =
    let len = List.length (List.hd l) in
    List.iter (fun x ->
      if List.length x <> len then raise (Bad_size (name ^ "of_list")))
    l;
    let tmp_mat = List.map Array.of_list l in
    {
      mat = Array.of_list tmp_mat;
      row = List.length l;
      col = len;
    }

  (* Output the matrix into a elt list list *)
  let to_list mat = Array.to_list (Array.map Array.to_list mat.mat)

  (* Create the matrix from [l] *)
  let of_array t =
    {
      mat = Array.map Array.copy t;
      row = Array.length t;
      col = Array.length t.(0);
    }
    
  (* Output the matrix into a elt array array *)
  let to_array mat = Array.map Array.copy mat.mat

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
  let copy mat =
    {
      mat = Array.init mat.row (fun i -> Array.copy mat.mat.(i));
      row = mat.row;
      col = mat.col;
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

  (* Set a value, modify the current matrix *)
  let set mat row col v =
    if row >= mat.row || col >= mat.col || row < 0 || col < 0 then
      raise (Bad_size (name ^ "set"));
    mat.mat.(row).(col) <- v;
    mat

  (* Iter on matrix elements with [f] *)
  let iter f mat = Array.iter (fun x -> Array.iter f x) mat.mat

  (* Map matrix [mat] with [f] *)
  let map f mat =
    {
      mat = Array.map (fun x -> Array.map f x) mat.mat;
      row = mat.row;
      col = mat.col;
    }

  (* Apply f on mat1 and mat2 *)
  let rec map2 f mat1 mat2 =
    if mat1.row <> mat2.row || mat1.col <> mat2.col then
      raise (Bad_size "Array_matrix.map2");
    let tmp_mat =
      Array.make_matrix mat1.row mat1.col (f mat1.mat.(0).(0) mat2.mat.(0).(0))
    in
    for i = 0 to mat1.row - 1 do
      for j = 0 to mat1.col - 1 do
        tmp_mat.(i).(j) <- (f mat1.mat.(i).(j) mat2.mat.(i).(j))
      done
    done;
    {
      mat = tmp_mat;
      row = mat1.row;
      col = mat1.col;
    }

  (* Create an identity matrix *)
  let identity size =
    let tmp_mat = Array.make_matrix size size Elt.zero in
    for i = 0 to size - 1 do
      tmp_mat.(i).(i) <- Elt.one
    done;
    {
      mat = tmp_mat;
      row = size;
      col = size;
    }

  (* Compute the trace *)
  let trace m =
    if m.row <> m.col then raise (Bad_size (name ^ "trace"));
    let res = ref Elt.zero in
    for i = 0 to m.row - 1 do
      res := Elt.add !res m.mat.(i).(i)
    done;
    !res
   
  (* Concat 2 matrix *)
  let concat m1 m2 =
    if m1.row <> m2.row then raise (Bad_size (name ^ "concat"));
    {
      mat = (
        Array.init (m1.col + m2.col) (fun i ->
          Array.append m1.mat.(i) m2.mat.(i))
      );
      row = m1.row;
      col = m1.col + m2.col;
    }

  (* Create a random matrix *)
  let random row col lim =
    Random.self_init ();
    {
      mat = 
        Array.init row (fun _ -> Array.init col (fun _ -> Elt.random lim));
      row = row;
      col = col;
    }

  (* Transpose the matrix *)
  let transpose mat =
    {
      mat = (
        Array.init mat.col (fun i ->
          Array.init mat.row (fun j -> mat.mat.(j).(i)))
      );
      row = mat.col;
      col = mat.row;
    }

  (* Print the matrix *)
  let print mat =
    Array.iter (fun x ->
      Array.iter (fun y ->
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
    let elem i j =
      let res = ref Elt.zero in
      for k = 0 to mat1.col - 1 do
        res := Elt.add !res (Elt.mul mat1.mat.(i).(k) mat2.mat.(k).(j))
      done;
      !res
    in
    {
      mat = (
        Array.init mat1.row (fun i ->
          Array.init mat2.col (fun j -> elem i j))
      );
      row = mat1.row;
      col = mat2.col;
    }

  (* Gauss method *)
  let gauss m =
    if m.row <> m.col then raise (Bad_size (name ^ "inverse"));
    let swap v i j =
      if i <> j then let t = v.(i) in v.(i) <- v.(j); v.(j) <- t
    in
    let res = Array.init m.row (fun i -> Array.copy m.mat.(i)) in
    let id = (identity m.row).mat in
    let prow = ref 0 in
    let pcol = ref 0 in
    while (!prow < m.row && !pcol < m.col ) do
      (* Find best pivot *)
      let maxp = ref Elt.zero in
      let pivotrow = ref 0 in
      for j = !prow to m.row - 1 do
        let av = Elt.abs res.(j).(!pcol) in
        if av > !maxp then (maxp := av; pivotrow := j)
      done;
      if !maxp <> Elt.eps then (
        (* Do pivot row swap *)
        swap res !prow !pivotrow;
        swap id !prow !pivotrow;
        (* Normalise current row *)
        let normfactor = Elt.div Elt.one res.(!prow).(!pcol) in
        for i = 0 to m.col - 1 do
          res.(!prow).(i) <- Elt.mul res.(!prow).(i) normfactor;
          id.(!prow).(i) <- Elt.mul id.(!prow).(i) normfactor;
        done;
        for j = 0 to m.row - 1 do 
          if j <> !prow then
            begin
              let tmp = res.(j).(!pcol) in
              for i = 0 to m.col - 1 do
                res.(j).(i) <-
                  Elt.sub res.(j).(i) (Elt.mul tmp res.(!prow).(i));
                id.(j).(i) <- Elt.sub id.(j).(i) (Elt.mul tmp id.(!prow).(i));
              done;
            end
        done;
        incr prow
      );
      incr pcol
     done;
     if m.row <> !prow then raise Not_inversible;
     {
       mat = id;
       row = m.row;
       col = m.col;
     }

  (* Invert a matrix *)
  let inverse = gauss

    (* Misc functions for Greville *)
  let dot_product v1 v2 =
    let array_fold_left2 f a v1 v2 =
      let r = ref a in
      for i = 0 to  (Array.length v1) - 1 do
        r:= f !r v1.(i) v2.(i)
      done;
      !r
    in
    array_fold_left2 (fun a b c -> Elt.add a (Elt.mul b c)) Elt.zero v1 v2

  let sub_mat m row col =
    {
      mat = (
        Array.init row (fun i ->
          Array.init col (fun j -> m.mat.(i).(j))));
      row = row;
      col = col;
    }

  let greville m =
    let res = transpose m in
    (* Init first line *)
    let v =
      {
        mat = [| Array.init m.row (fun i -> m.mat.(i).(0)) |];
        row = 1;
        col = m.row;
      }
    in
    let r = ref (dot_product v.mat.(0) v.mat.(0)) in (* Norm of v *)
    for i = 0 to m.row - 1 do
      res.mat.(0).(i) <- (
        if Elt.abs !r > Elt.eps then Elt.div m.mat.(i).(0) !r
        else Elt.zero)
    done;
    for k = 1 to m.col - 1 do
      for i = 0 to m.row - 1 do
        v.mat.(0).(i) <- m.mat.(i).(k)
      done;
      let d = mul (sub_mat res k m.row) (transpose v) in
      let x = mul (sub_mat m m.row k) d in
      let c = sub v (transpose x) in
      r := dot_product c.mat.(0) c.mat.(0); (* Norm of c *)
      if Elt.abs !r > Elt.eps then begin (* Matrix is inversible *)
        for i = 0 to m.row - 1 do
          v.mat.(0).(i) <- Elt.div c.mat.(0).(i) !r
        done
      end
      else begin (* Matrix is not inversible *)
        let trans_d = transpose d in
        let tmp_d = inverse (add_elem (mul trans_d d) Elt.one) in
        let not_inv = mul (mul tmp_d trans_d) (sub_mat res k m.row) in
        for i = 0 to m.row - 1 do
          v.mat.(0).(i) <- not_inv.mat.(0).(i)
        done
      end;
      (* Build the pseudo inverse line by line *)
      for i = 0 to res.col - 1 do
        for j = 0 to k - 1 do
          res.mat.(j).(i) <-
            Elt.sub res.mat.(j).(i) (Elt.mul d.mat.(j).(0) v.mat.(0).(i))
        done
      done;
      for i = 0 to res.col - 1 do
        res.mat.(k).(i) <- v.mat.(0).(i)
      done;
    done;
    res
        
  (* Penrose pseudo inverse *)
  let pseudo_inverse = greville



end

(******************************)
(* Define some useful modules *)
(******************************)

module IntArrayMatrix = ArrayMatrix (Int)
module FloatArrayMatrix = ArrayMatrix (Float)
