open Ppl
open Core

let coin_tossing =
  let weight : prob dist =
    let* isFair = bernoulli 0.8 in
    if isFair then return 0.5 else beta 5. 1.
  in
  let toss d b = condition' (fun w -> if b then w else 1. -. w) d in
  let tosses bs d = List.fold bs ~init:d ~f:toss in
  let observations =
    [ true; false; true; true; false; true; true; true; false ]
  in
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
