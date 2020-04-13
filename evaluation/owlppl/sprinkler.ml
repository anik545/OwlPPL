open Ppl

(* The example used in hansei *)
let grass_model i () =
  let model =
    condition' (fun (w,_) -> if w then 1. else 0.)
      (let* cloudy = bernoulli 0.5 in
       let* rain = bernoulli (if cloudy then 0.8 else 0.2) in
       let* sprinkler = bernoulli (if cloudy then 0.1 else 0.5) in
       let wet_grass = rain || sprinkler in
       return (wet_grass, rain))
  in
  let d = infer model i in
  sample_n 1000 d
