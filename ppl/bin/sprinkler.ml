open Ppl


let grass_model = 
  let (let+) cond dist = Conditional(cond, dist()) in
  (* let (&&) = liftM2 (&&) in
     let (||) = liftM2 (||) in *)
  let* rain = bernoulli 0.3 in
  let* sprinkler = bernoulli 0.5 in
  let* grass_is_wet = bernoulli (if rain then 0.9 else if sprinkler then 0.8 else 0.1) in
  (* let grass_is_wet = bernoulli 0.9 && rain 
                     || bernoulli 0.8 && sprinkler
                     || bernoulli 0.1  *)
  let+ _ = fun _ -> if grass_is_wet then 1. else 0. in (* make this look nicer *)
  return (rain,sprinkler)

let model = 
  let* burglary = bernoulli 0.00 in 
  let* alarm = bernoulli (if burglary then 0.95 else 0.01) in
  return (alarm, burglary)