
open Ppl
open Core

let single_coin = 
  let pr = c_uniform 0. 1. in
  let toss t = condition' (fun p -> Primitives.(pdf @@ binomial 10 p) t) in
  let obs = 9 in (* we see x=9 heads *)
  let posterior = toss obs pr in
  (* let posterior' = condition (fun p -> pdf (Binomial(10,p)) obs) (c_uniform 0. 1.) in *)
  posterior
(* exact posterior:  Beta(x+ 1; n-x+ 1) *)

let flip = bernoulli
let grass_model = fun () ->
  let* cloudy    = flip 0.5 in
  let* rain      = flip (if cloudy then 0.8 else 0.2) in
  let* sprinkler = flip (if cloudy then 0.1 else 0.5) in
  (* let* a = flip 0.7 in *)
  let* b = flip 0.9 in
  let* c = flip 0.9 in
  (* let wet_roof  = a && rain in *)
  let wet_grass = b && rain || c && sprinkler in
  condition wet_grass 
    (return rain)
;;
