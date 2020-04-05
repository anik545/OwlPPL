open Core
open Ppl

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  let exec_time = (t1 -. t) in
  res, exec_time*.1000.

let ignore_output f = fun x y -> let _ = f x y in ()

let model = (Sys.get_argv ()).(1)
let model = match model with
  | "hmm" -> ignore_output Hmm.hmm_model
  | "linreg" -> ignore_output Linreg.linreg_model
  | "dpmm" -> ignore_output Dpmm.dpMixture
  | "coin" -> ignore_output Coin.coin_mean
  | _ -> raise @@ Invalid_argument model


let inference = (Sys.get_argv ()).(2)
let inference_alg = match inference with
  | "mh" -> MH(100)
  | "smc" -> MH(100)
  | "rej" -> Rejection(100,Hard)
  | "imp" -> Importance(100)
  | "pimh" -> PIMH(100)
  | "pc" -> PC(100)
  | "exact" -> Enum
  | _ -> raise @@ Invalid_argument inference

let _,t = time (model inference_alg)

let () = Printf.printf "%f\n" t
