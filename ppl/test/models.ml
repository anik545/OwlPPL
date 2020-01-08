
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
