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

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  let exec_time = (t1 -. t) in
  Printf.printf "Execution time: %f seconds\n" exec_time;
  res, exec_time*.1000.

let model = 
  try (Sys.get_argv ()).(1)
  with Invalid_argument _ -> "mh"
let i = match model with
    "mh" -> MH(100)
  | "smc" -> MH(100)
  | "rej" -> Rejection(100,Hard)
  | "imp" -> Importance(100)
  | "pimh" -> PIMH(100)
  | "pc" -> PC(100)
  | "exact" -> Enum
  | _ -> raise @@ Invalid_argument model

let mn,t = time (hmm_model i)

let () = Printf.printf "%f\n" t
