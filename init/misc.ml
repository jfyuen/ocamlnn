module FM = Matrix

(* norme [v] : [v] is a vector, compute its norm *)
let norme v = 
   let rec norme_ cpt accu =  
      let (row, col) = FM.size v in
	match cpt with 
	n when n = row -> accu
	| _ -> 
	(
	 let x = FM.get v cpt 0 in
	   norme_ (cpt + 1) (accu +. (x *. x)) 
	)
in
   sqrt(norme_ 0 0.)
;;

let get_col m nb = 
    let rec get_col_ cpt accu = 
	match cpt with 
	n when n = fst (FM.size m) -> accu
	| _ -> get_col_ (cpt + 1) (accu@[[FM.get m cpt nb]])
    in
    FM.of_list ((get_col_ 0 []))
;;


let compute_mean m nb = 
    let col = get_col m nb in
      let rec compute_mean_ cpt accu_mean = 
	match cpt with 
	n when n = fst (FM.size m) -> accu_mean /. (float_of_int n) 
        | _ -> compute_mean_ (cpt + 1) (accu_mean +. FM.get col cpt 0) 
      in
    compute_mean_ 0 0.
;;

let center m = 
    let rec sub_mean m cpt col mean = 
	match  cpt with 
      	n when n = fst (FM.size m) -> m 
	| _ -> sub_mean (FM.set m cpt col 
		((FM.get m cpt col) -. mean)) (cpt + 1) col mean
     in 
     let rec center_ m col = 
	match col with 
        n when n = snd (FM.size m) -> m
	| _ -> center_ (sub_mean m 0 col (compute_mean m col)) (col + 1)
     in 
     center_ m 0
;;

(**)
let compute_ecart_type m nb = 
     let col = get_col m nb in
     let mean = compute_mean m nb in 
     let rec compute_ cpt accu = 
	match cpt with 
        n when n = fst (FM.size col) -> sqrt(accu /. (float_of_int n))
       | _ -> compute_ (cpt + 1) (accu +. ((FM.get col cpt 0) -. mean) ** 2.)
     in
     compute_ 0 0.
;;

(**) 
let reduce m = 
     let rec div_ecart_type m cpt col ecart = 
	match  cpt with 
      	n when n = fst (FM.size m) -> m 
	| _ -> div_ecart_type (FM.set m cpt col 
		((FM.get m cpt col) /. ecart)) (cpt + 1) col ecart
     in 
     let rec reduce_ m col = 
	match col with 
        n when n = snd (FM.size m) -> m
	| _ -> (
	let ecart = 
	   match compute_ecart_type m col with 
	   0. -> 1.
	 | n -> n 
	in	
	  reduce_ (div_ecart_type m 0 col  ecart ) (col + 1)
	)	
     in 
     reduce_ m 0
;;


let center_reduce m = reduce(center m);;


let compute_m m = 
 let make_list col = 
    let res = ref [] in 
    for i = 0 to fst(FM.size col ) - 1 do
      res := (!res)@[FM.get col i 0]
    done;
    !res
  in
  let rec compute_ accu = function
    | [] -> accu
    | h::t -> compute_ (accu@[(make_list h)]) t
  in
 (FM.of_list (compute_ [] m))


let get_res x = 
  let (m, n) = FM.size x in 
  let get_line x line = 
   let res = ref [] in 
   for i = 0 to n - 1  do
    res := (!res)@[[FM.get x line i ]];
   done;
   FM.of_list !res
   in 
   let res = ref [] in 
   for i = 0 to m - 1 do
   res := (!res)@[(get_line x i)];
   done;
   !res 
   

let center_it x = 
  let m = compute_m x in 
  let res = center m in 
  let res2 = get_res res  in 
  res2
