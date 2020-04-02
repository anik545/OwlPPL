let coin_mean inference_algorithm =
  let coin heads =
    let* weight = continuous_uniform 0. 1. in
    observe heads (binomial 10 weight)
      (return weight)
  in

  let posterior_single_coin = infer (coin 9) inference_algorithm in
  sample_mean ~n:10000 (posterior_single_coin) (* 0.833 *)
