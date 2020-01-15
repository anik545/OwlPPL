(* open Common *)
open Core
open Dist.GADT_Dist
(* metropolis-hastings *)
(* 
For mh, we need a proposal distribution to start choosing values from,
and a function *proportional* to the density (here the scores assigned by `prior`)
*)
let mh n d =
  let proposal = prior1 d in
  let rec iterate ?(n=n) (x,s) =
    if n = 0 then return [] else
      let* (y,r) = proposal in
      let ratio = if Float.(s = 0.) then 1. else r /. s in
      let* accept = bernoulli @@ Float.min 1. ratio in
      let next = if accept then (y,r) else (x,s) in
      let* rest = iterate ~n:(n-1) next in
      return (next::rest)
  in
  fmap (List.map ~f:fst) (let* x = proposal in iterate x)

let mh' n d = fmap (fun x -> List.nth_exn x (n-1)) (mh n d)

(* Don't generate entire list - only care about distribution over final state *)
let mh'' n d =
  let proposal = prior1 d in
  let rec iterate ?(n=n) (x,s) =
    if n = 0 then return (x,s) else
      let* (y,r) = proposal in
      let ratio = if Float.(s = 0.) then 1. else r /. s in
      let* accept = bernoulli @@ Float.min 1. ratio in
      let next = if accept then (y,r) else (x,s) in
      let* final = iterate ~n:(n-1) next in
      return (final)
  in
  fmap fst (let* x = proposal in iterate x)


(* particle independent metropolis hastings *)
let pimh n d = mh n (Smc.smc n d)
let pimh' k n d = mh' k (Smc.smc' n d)

