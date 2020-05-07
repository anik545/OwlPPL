open Ppl
open Core

let flip = bernoulli

let grass_model =
  let d =
    condition'
      (fun (w, _) -> if w then 1. else 0.)
      (let* cloudy = flip 0.5 in
       let* rain = flip (if cloudy then 0.8 else 0.2) in
       let* sprinkler = flip (if cloudy then 0.1 else 0.5) in
       (* let* a = flip 0.7 in *)
       let* b = flip 0.9 in
       let* c = flip 0.9 in
       (* let wet_roof  = a && rain in *)
       let wet_grass = (b && rain) || (c && sprinkler) in
       return (wet_grass, rain))
  in
  fmap snd d

let grass_model_exact =
  Primitive.categorical [ (true, 0.704225); (false, 0.295775) ]

let x = take_k_samples 100 @@ cascade' 100 grass_model
