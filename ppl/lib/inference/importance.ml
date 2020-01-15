(* open Common *)
open Core
open Dist.GADT_Dist
open Common
(* likelihood weighting *)
let importance n d = sequence @@ List.init n ~f:(fun _ -> prior d)
let importance' n d = 
  let* l = (importance n d) in 
  categorical l


let rec create' n d sofar =
  if n = 0 then sofar
  else 
    match sample d with
    | Some x -> create' (n-1) d (x::sofar)
    | None -> create' n d sofar

let create n d = create' n d []

let reject_transform_hard ?(threshold=0.) d = 
  let rec repeat () =
    let* (x,s) = prior1 d in
    if Float.(s > threshold) then return (x,s) else repeat ()
  in
  repeat ()

let reject_transform_soft d = 
  let rec repeat () =
    let* (x,s) = prior1 d in
    let* accept = bernoulli s in
    if accept then return (x,s) else repeat ()
  in
  repeat ()

let rejection_transform ?(n=10000) (s:[> `Hard | `Soft]) d = 
  let reject_dist = match s with 
    | `Hard -> reject_transform_hard ~threshold:0. 
    | `Soft -> reject_transform_soft 
  in
  (sequence @@ List.init n ~f:(fun _ -> (reject_dist d))) >>= categorical

let rejection_soft d = 
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
  (* List.init n ~f:(fun _ -> sample (reject_dist d))
     |> List.filter ~f:(is_some)
     |> List.filter_opt *)
  create n (reject_dist d)
  |> unduplicate
  |> normalise
  |> categorical

(* 
let rec create d n =
  if n = 0 then [] 
  else 
    match sample d with
    | Some x -> x::(create d (n-1))
    | None -> create d n *)

