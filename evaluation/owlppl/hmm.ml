open Ppl
open Core

let trans s = if s then bernoulli 0.7 else bernoulli 0.3
let observe s = if s then bernoulli 0.9 else bernoulli 0.1

type 'a hmm_model = {states:'a list; observations: 'a list}

let hmm_model inf () = 
  let rec hmm n =
    let* prev = match n with
        1 -> return ({states=[true];observations=[]})
      | _ -> hmm (n-1)
    in

    let* new_state = trans (List.hd_exn prev.states) in
    let* new_obs = observe new_state in
    return ({states=(new_state::prev.states);observations=(new_obs::(prev.observations))})
  in
  let model =
    let obs = [false;false;false] in
    let* r = hmm 3 in
    condition (Stdlib.(r.observations = obs))
      (return @@ List.rev r.states)
  in
  let m = infer model inf in
  Array.init 1000 ~f:(fun _ -> sample m)