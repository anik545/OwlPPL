open Ppl
open Core

let coin_tossing () =
  let weight: prob dist =
    let* isFair = bernoulli 0.8 in
    if isFair then return 0.5 else beta 5. 1.
  in

  let toss b d = condition (fun w -> if b then w else 1. -. w) d in

  let tosses bs d = List.fold bs ~init:d ~f:(flip toss) in


  let observations = [false; false; false; false; false; false; false] in
  let posterior_weight = tosses observations weight in
  posterior_weight
