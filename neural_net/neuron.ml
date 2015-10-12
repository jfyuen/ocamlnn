type neuron =
{
  input : (float * float) array; (* weight, old dw *)
  mutable output : float;
  mutable activation : float;
  deltae : (float * float) array; (* gradient, old gradient *)
  update : float array; (* Update value for each weight *)
}

type algo =
  | Backprop of float * float
  | Quickprop of float * float * float * float
  | Rprop of float * float * float * float
  | SuperSAB of float * float * float * float * bool

(* Create a neuron *)
let create input t out update =
  {
    input = Array.append [|(t, 0.)|] input;
    output = out;
    activation = 0.;
    deltae = Array.make (Array.length input + 1) (0., 0.);
    (* Default value: 0.1 *)
    update = Array.make (Array.length input + 1) update;
  }

let print node =
  Printf.printf "output: %f\n" node.output;
  Printf.printf "Inputs: ";
  for i = 0 to Array.length node.input - 1 do
    Printf.printf "%f " (fst node.input.(i));
  done;
  print_newline ()

let apply_weights node = function
  | Backprop (lrate, moment) ->
      for k = 0 to Array.length node.input - 1 do
        let w = fst node.input.(k) in
        let m = snd node.input.(k) in
        let delta = lrate *. fst node.deltae.(k) +. m *. moment in
        let weight = w +. delta in
        node.input.(k) <- (weight, delta);
        node.deltae.(k) <- (0., 0.)
      done
  | Quickprop (lrate, moment, mu, decay) ->
      let shrink = mu /. (1. +. mu) in
      for k = 0 to Array.length node.input - 1 do
        (* Apply Quickprop weight update *)
        let dw = ref 0. in
        let old_dw = snd node.input.(k) in
        let weight = fst node.input.(k) in
        let delta = fst node.deltae.(k) -. decay *. weight in
        let old_delta = snd node.deltae.(k) in
        if old_dw > 0. then begin
          if delta > 0. then dw := lrate *. delta;
          if delta > shrink *. old_delta then dw := !dw +. mu *. old_dw
          else dw := !dw +. (delta /. (old_delta -. delta)) *. old_dw
        end else if old_dw < 0. then begin
          if delta < 0. then dw := lrate *. delta;
          if delta < shrink *. old_delta then dw := !dw +. mu *. old_dw
          else dw := !dw +. (delta /. (old_delta -. delta)) *. old_dw
        end else dw := lrate *. delta;
        (* Apply weights modification *)
        let w = weight +. !dw -. decay *. weight in
        node.input.(k) <- (w, !dw);
        node.deltae.(k) <- (0., delta)
      done
  | Rprop (eta_plus, eta_minus, d_max, d_min) ->
      for k = 0 to Array.length node.input - 1 do
        (* have simple names, set values *)
        let dij = ref 0. in
        let dw = ref 0. in
        let old_dij = node.update.(k) in
        let wij = fst node.input.(k) in
        let delta = fst node.deltae.(k) in
        let old_delta = snd node.deltae.(k) in
        let tmp_delta =
          (old_delta > 0. && delta > 0.) || (old_delta < 0. && delta < 0.) in
        let tmp_delta2 =
          (old_delta > 0. && delta < 0.) || (old_delta < 0. && delta > 0.) in
        (* Apply Rprop weight update *)
        if tmp_delta then begin
          dij := min (old_dij *. eta_plus) d_max;
          dw := if delta > 0. then !dij else -. !dij;
          node.update.(k) <- !dij;
          node.deltae.(k) <- (0., delta)
        end else if tmp_delta2 then begin
          dij := max (old_dij *. eta_minus) d_min;
          node.update.(k) <- !dij;
          node.deltae.(k) <- (0., 0.)
        end else begin
          dw := if delta > 0. then old_dij else -. old_dij;
          node.deltae.(k) <- (0., delta)
        end;
        (* Update to new values *)
        node.input.(k) <- (wij +. !dw, !dw)
      done
  | SuperSAB (moment, u, d, eta_max, inc_err) ->
      (* node.update used for each lrate *)
      for k = 0 to Array.length node.input - 1 do
        let delta = fst node.deltae.(k) in
        let old_delta = snd node.deltae.(k) in
        let wij = fst node.input.(k) in
        let old_dw = snd node.input.(k) in
        if inc_err then begin
          node.update.(k) <- node.update.(k) *. 0.5;
          node.input.(k) <- (wij -. old_dw, 0.);
          node.deltae.(k) <- (0., 0.)
        end else begin
          let lrate = node.update.(k) in
          node.update.(k) <-
            if delta *. old_delta >= 0. then
              if lrate < eta_max then lrate *. u else eta_max
            else lrate *. d;
          let dw = lrate *. delta +. moment *. old_dw in
          node.input.(k) <- (wij +. dw, dw);
          node.deltae.(k) <- (0., delta)
        end;
      done
