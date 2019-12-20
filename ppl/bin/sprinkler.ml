open Ppl


let grass_model = 
  let (&&) = liftM2 (&&) in
  let (||) = liftM2 (||) in
  
  let rain = bernoulli 0.3 in
  let sprinkler = bernoulli 0.5 in
  let grass_is_wet = bernoulli 0.9 && rain 
                    || bernoulli 0.8 && sprinkler
                    || bernoulli 0.1 
  in
  grass_is_wet