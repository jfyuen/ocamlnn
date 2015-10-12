module M = Matrix
open Init_intf

module Init (A : ALGO) = struct

  let get_nn nc l =
    Random.self_init ();
    let tmp_lst = List.map (fun x -> M.to_list (M.transpose x)) l in
    let lst = List.map List.hd tmp_lst in
    assert (nc.(0) = List.length (List.hd lst));
    let len = List.length l in
    let mk_rand_array t =
      for i = 0 to Array.length t - 1 do
        let mul = if Random.float 1. < 0.5 then 1. else -1. in
        let rnd = Random.float max_float /. max_float in
        t.(i) <- rnd *. mul
      done
    in
    let mk_list_matrix size n =
      let rand_array = Array.make size 0. in
      let rec mk_list acc = function
        | 0 -> acc
        | n ->
            mk_rand_array rand_array;
            mk_list (List.hd (M.to_list (
              M.of_array [|rand_array|])) :: acc) (n - 1)
      in
      mk_list [] n
    in
    (* First inputs in the nn *)
    let inputs = lst @ (mk_list_matrix nc.(0) (nc.(1) - len)) in
    (* Create the rest of the list thing *)
    let tab = Array.init (Array.length nc - 2) (fun i ->
      mk_list_matrix nc.(i + 1) nc.(i + 2)) in
    Array.append [|inputs|] tab

  let perform = A.perform

  let version = A.version

end
    
module AcpInit = Init (Acp)
module GhaInit = Init (Gha)
module ApexInit = Init (Apex)
module AlaInit = Init (Ala)
module RandomInit = Init(Rand)
