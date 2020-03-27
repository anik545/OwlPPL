let coin_mean inference_algorithm = 
  let coin heads = 
    let posterior = condition' (fun p -> Primitives.(pdf @@ binomial 10 p) heads) (continuous_uniform 0. 1.) in
    posterior
  in

  let posterior_single_coin = infer (coin 9) inference_algorithm in
  sample_mean ~n:10000 (posterior_single_coin) (* 0.833 *)

