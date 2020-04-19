open Ppl
open Core

(* BIASED COIN *)
(* coin, see 9 heads out of 10 flips *)
let single_coin =
  let pr = continuous_uniform 0. 1. in
  let toss t = condition' (fun p -> Primitive.(pdf @@ binomial 10 p) t) in
  let obs = 9 in
  (* we see x=9 heads *)
  let posterior = toss obs pr in
  (* let posterior' = condition (fun p -> pdf (Binomial(10,p)) obs) (continuous_uniform 0. 1.) in *)
  posterior

(* exact posterior:  Beta(x+ 1; n-x+ 1) *)
(* here, x=9, n=10 *)
let single_coin_exact = Primitive.beta 10. 2.

(* SPRINKLER *)
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

(* HMM *)

(* linear regression *)

let linreg obs =
  let linear =
    let* a = normal 0. 2. in
    let* b = normal 0. 2. in
    let* c = gamma 1. 1. in
    return (a, b, c)
  in
  let open Float in
  let point d (x, y) =
    condition' (fun (a, b, c) -> Primitive.(pdf @@ normal ((a * x) + b) c) y) d
  in
  let points ps d = List.fold ~f:point ~init:d ps in
  (* let obs = List.init 10 ~f:(fun x -> let x = float_of_int x in (x,x*.2.)) in *)
  let posterior = points obs linear in
  posterior
