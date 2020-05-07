open Core
open Ppl

let ignore_output f x y =
  let _ = f x y in
  ()

let model = (Sys.get_argv ()).(1)

let model =
  match model with
  | "hmm" -> ignore_output Hmm.hmm_model
  | "linreg" -> ignore_output Linreg.linreg_model
  | "dpmm" -> ignore_output Dpmm.dpmm
  | "coin" -> ignore_output Coin.coin_mean
  | "sprinkler" -> ignore_output Sprinkler.grass_model
  | _ -> raise @@ Invalid_argument model

let inference = (Sys.get_argv ()).(2)

let inference_alg =
  match inference with
  | "mh" -> MH 1000
  | "smc" -> SMC 20
  | "rej" -> Rejection (4000, Hard)
  | "imp" -> Importance 1000
  | "pimh" -> PIMH 100
  | "pc" -> PC 100
  | "exact" -> Enum
  | _ -> raise @@ Invalid_argument inference

let _, t = time (model inference_alg)

let () = Printf.printf "%.15f\n" t
