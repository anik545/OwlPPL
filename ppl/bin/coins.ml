open Ppl
open Core

let coin_tossing =
  let weight: prob dist =
    let* isFair = bernoulli 0.8 in
    if isFair then return 0.5 else beta 5. 1.
  in

  let toss b d = condition (fun w -> if b then w else 1. -. w) d in

  let tosses bs d = List.fold bs ~init:d ~f:(flip toss) in


  let observations = [true; false; true; true; false; true; true; true; false] in
  let posterior_weight = tosses observations weight in
  posterior_weight

let post = mh' 1000 coin_tossing

let m () = sample_mean ~n:100 post
let m' = m ()

let hist = Owl_stats.(histogram (`N 50) (take_k_samples 1000 post))
(* 
let () = 
  let pl = hist_dist ~n:1000 ~fname:"a.png" post in
  Owl_plplot.Plot.output pl;
  () *)
(* --- *)
(* https://www.cl.cam.ac.uk/teaching/1819/DataSci/notes0.pdf pg33 *)

let single_coin = 
  let pr = c_uniform 0. 1. in

  let toss t = condition (fun p -> pdf (Binomial(10,p)) t) in

  let obs = 9 in (* we see x=9 heads*)

  let posterior = toss obs pr in
  let posterior' = condition (fun p -> pdf (Binomial(10,p)) obs) (c_uniform 0. 1.) in
  posterior,posterior'

let post_single_coin = mh' 500 @@ fst single_coin 
let m = sample_mean (post_single_coin)
let m' = sample_mean (post_single_coin)

let () = 
  let pl = hist_dist ~n:5000 ~fname:"b.png" post_single_coin in
  Owl_plplot.Plot.output pl;
