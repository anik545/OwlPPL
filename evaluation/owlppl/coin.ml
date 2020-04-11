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
