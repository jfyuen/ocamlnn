module FM = Matrix


let version = "ACP"
;;

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
         

let compute_error (vp, nvp) = 
  let res = FM.sub vp nvp in 
     norme res
;;
	 
let epsilon = 1e-10;;

(* it_power [M] : iteratives power method to compute vetcuers propres *) 
let it_power m = 
   let vp_tmp =  FM.random (fst (FM.size m)) 1 (10.) in
   let vp = FM.add_elem vp_tmp 1. in
   let rec it_power_ vp cpt =
      let tmp_vp = FM.mul m vp in
        let new_vp = FM.div_elem tmp_vp (norme tmp_vp) in
          match vp with 
            n when (compute_error (n,new_vp) < epsilon
          || cpt > 15000)-> 
              new_vp
         | _ -> it_power_ new_vp  (cpt + 1)
   in
   it_power_ vp 0 
;; 

(*L1 = q1^t M q1*)
let compute_value m vp =
	let vptM =  FM.mul  (FM.transpose vp) m in 
	 FM.get (FM.mul vptM vp) 0 0
;;
   
let compute_value2 m vp = 
    FM.get (FM.mul m (FM.div_elem vp (FM.get vp 0 0))) 0 0
;;

(***************************************)
(*********        ACP      *************)
(***************************************)

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

let correlation m = 
   let r = center_reduce m in
    FM.mul  (FM.transpose r) r
;;

let var_covar m = 
  let r = center m in 
    let res = FM.mul (FM.transpose r) r  in
res 
;;
 
let acp m vp_get = 
    let r = var_covar m (*correlation m*) in 
      vp_get r
;;
	
		
let get_vp_itpower nb_vect precision m =  
    let t = FM.trace m in
	let rec get_vp_ m cpt accu cpt2 = 
	   match cpt with 
	   n when (n >= precision *. t) || (cpt2 = nb_vect)-> accu 
	  | _ -> (
		let vp = it_power m in 
		let value = compute_value m vp in 
		get_vp_ (FM.sub m (FM.mul_elem 
			(FM.mul vp (FM.transpose vp)) value)) 
			(cpt +. value) (accu@[vp]) (cpt2 + 1)
	)
	in
	get_vp_ m 0. [] 0
;;



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
;;

let perform m nb_vect = 
  let mat  = compute_m m in
  let res = acp mat (get_vp_itpower nb_vect 1.) in 
  (*List.iter (fun x -> FM.print x; print_newline()) res; 
  prerr_endline "done";*)
  res

