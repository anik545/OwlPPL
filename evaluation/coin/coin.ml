open Ppl
open Core
let coin_mean inf () = 
  let coin heads = 
    let posterior = condition' (fun p -> Primitive.(pdf @@ binomial 10 p) heads) (continuous_uniform 0. 1.) in
    posterior
  in

  let post_single_coin = infer (coin 9) inf in
  let mn  = sample_mean ~n:10000 (post_single_coin) (* 0.833 *) in
  mn

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
  | "smc" -> SMC(100)
  | "rej" -> Rejection(100,Hard)
  | _ -> raise @@ Invalid_argument model

let mn,t = time (coin_mean i)
(* let times = Array.init 10 ~f:(fun _ -> snd @@ time coin_mean) *)
let () = Printf.printf "%f\n%f\n" mn t
(* let () = Array.iter ~f:(printf "%f,") times *)
