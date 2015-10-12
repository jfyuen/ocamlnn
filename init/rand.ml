open Matrix 
let version = "Random"

let perform x nb_vect =
  let mk_rand_array t =
    for i = 0 to Array.length t - 1 do
      let mul = if Random.float 1. < 0.5 then -1. else 1. in
      let rnd = Random.float max_float /. max_float in
      t.(i) <- [|rnd *. mul |]
    done in
  let len = fst(size (List.hd x)) in	
  let rand_array = Array.make len [||] in
  let rec make_list accu cpt = 
    if cpt = nb_vect then accu 
    else begin
      mk_rand_array rand_array;
      make_list (of_array rand_array::accu) (cpt + 1)
    end in 
  make_list [] 0
;;

    
