type file_contents =
  {
    layers : int array;
    activation : string;
    init : string;
    gn : bool;
    learning : string;
    threshold : float;
    vals : float array;
    max_iter : int;
    eps : float;
    stable_eps : float;
    stable_iter : int;
  }

let file_to_list name =
    let ic = open_in name in
    let rec file_to_list_ l =
      try
        let line = input_line ic in
        if String.length line > 0 && line.[0] = '#' then file_to_list_ l
        else file_to_list_ (line :: l)
      with
      | End_of_file -> close_in ic; l in
    file_to_list_ []

let read_conf name =
  try
    Random.self_init ();
    let get_elems pattern =
      List.find (fun s -> Str.string_match (Str.regexp pattern) s 0) in
    let split s = Str.split (Str.regexp s) in
    let get_string elm = List.hd (List.tl (split "=" elm)) in
    let l = file_to_list name in
    let layers = get_elems "^\\[neural_net\\]=" l in
    let activ = get_string (get_elems "^\\[Activation\\]=" l) in
    let learning = get_string (get_elems "^\\[learning\\]" l) in
    let init = get_string (get_elems "^\\[init\\]" l) in
    let gn = bool_of_string (get_string (get_elems "^\\[gauss-newton\\]" l)) in
    let lrate = float_of_string (get_string (get_elems "^\\[lrate\\]" l)) in
    let moment =
      float_of_string (get_string (get_elems "^\\[momentum\\]" l)) in
    let mu = float_of_string (get_string (get_elems "^\\[mu\\]" l)) in
    let decay = float_of_string (get_string (get_elems "^\\[decay\\]" l)) in
    let d_max = float_of_string (get_string (get_elems "^\\[d_max\\]" l)) in
    let d_min = float_of_string (get_string (get_elems "^\\[d_min\\]" l)) in
    let update = float_of_string (get_string (get_elems "^\\[d_0\\]" l)) in
    let eta_minus =
      float_of_string (get_string (get_elems "^\\[eta_minus\\]" l)) in
    let eta_plus =
      float_of_string (get_string (get_elems "^\\[eta_plus\\]" l)) in
    let max_iter =
      int_of_string (get_string (get_elems "^\\[max_iter\\]" l)) in
    let eps = float_of_string (get_string (get_elems "^\\[eps\\]" l)) in
    let stable_iter =
      int_of_string (get_string (get_elems "^\\[stable_iter\\]" l)) in
    let stable_eps =
      float_of_string (get_string (get_elems "^\\[stable_eps\\]" l)) in
    let threshold = (get_string (get_elems "^\\[threshold\\]" l)) in
    let t =
      if threshold = "Random" then begin
        let rnd = Random.float max_float /. max_float in
        let s = if Random.float 1. < 0.5 then -1. else 1. in
        s *. rnd
      end else float_of_string threshold in
    let u = float_of_string (get_string (get_elems "^\\[u\\]" l)) in
    let d = float_of_string (get_string (get_elems "^\\[d\\]" l)) in
    let eta_max =
      float_of_string (get_string (get_elems "^\\[eta_max\\]" l)) in
    let nb =
      Array.of_list
      (List.map int_of_string
      (split "[, \t]+" (List.hd (List.tl (split "=" layers))))) in
    {
      layers = nb;
      activation = activ;
      init = init;
      gn = gn;
      learning = learning;
      threshold = t;
      vals =
        [| lrate; moment; mu; decay; eta_plus; eta_minus; d_max; d_min;
        update; u; d; eta_max |];
      max_iter = max_iter;
      eps = eps;
      stable_iter = stable_iter;
      stable_eps = stable_eps;
    }
  with _ -> failwith "Wrong config file."

