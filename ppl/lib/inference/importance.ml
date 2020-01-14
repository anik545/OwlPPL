(* open Common *)
open Core
open Dist.GADT_Dist
open Common
(* likelihood weighting *)
let importance n d = sequence @@ List.init n ~f:(fun _ -> prior d)
let importance' n d = 
  let* l = (importance n d) in 
  categorical l


let rejection_soft  d = 
  let* (x,s) = prior1 d in
  let* accept = bernoulli s in
  if accept then return (Some (x,s)) else return None

let rejection_hard ?(threshold=0.) d = 
  let* (x,s) = prior1 d in
  if Float.(s > threshold) then return (Some (x,s)) else return None

let rejection ?(n=10000) (s:[> `Hard | `Soft]) d = 
  let reject_dist = match s with 
    | `Hard -> rejection_hard ~threshold:0. 
    | `Soft -> rejection_soft 
  in
  List.init n ~f:(fun _ -> sample (reject_dist d))
  |> List.filter ~f:(is_some)
  |> List.filter_opt
  |> unduplicate
  |> normalise
  |> categorical

