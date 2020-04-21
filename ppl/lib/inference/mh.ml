(* open Common *)
open Core
open Dist

(* metropolis-hastings *)
(* 
For mh, we need a proposal distribution to start choosing values from,
and a function *proportional* to the density (here the scores assigned by `prior`)
*)
open Dist.Prob

let mh n d =
  let proposal = prior_with_score d in
  let rec iterate ?(n = n) (x, s) =
    if n = 0 then return []
    else
      let* y, r = proposal in
      let ratio = if Float.(s = zero) then one else r /. s in
      let* accept = bernoulli @@ Float.min one ratio in
      let next = if accept then (y, r) else (x, s) in
      let* rest = iterate ~n:(n - 1) next in
      return (next :: rest)
  in
  fmap (List.map ~f:fst)
    (let* x = proposal in
     iterate x)

let mh' n d = fmap (fun x -> List.nth_exn x (n - 1)) (mh n d)

(* Don't generate entire list - only care about distribution over final state *)
let mh'' n d =
  let proposal = prior_with_score d in
  let rec iterate ?(n = n) (x, s) =
    if n = 0 then return (x, s)
    else
      let* y, r = proposal in
      let ratio = if Float.(s = zero) then one else r /. s in
      let* accept = bernoulli @@ Float.min one ratio in
      let next = if accept then (y, r) else (x, s) in
      let* final = iterate ~n:(n - 1) next in
      return final
  in
  fmap fst
    (let* x = proposal in
     iterate x)

let mh_sampler n d =
  let proposal = prior_with_score d in
  let rec iterate ?(n = n) (x, s) =
    if n = 0 then return []
    else
      let* y, r = proposal in
      let ratio = if Float.(s = zero) then one else r /. s in
      let* accept = bernoulli @@ Float.min one ratio in
      let next = if accept then (y, r) else (x, s) in
      let* rest = iterate ~n:(n - 1) next in
      return (next :: rest)
  in
  fmap (List.map ~f:fst)
    (let* x = proposal in
     iterate x)

(* particle independent metropolis hastings *)
let pimh n d = mh n (Smc.smc n d)

let pimh' k n d = mh' k (Smc.smc' n d)

let mh_sampler' ~burn d =
  let open Core.Sequence.Step in
  let proposal = prior_with_score d in
  let iterate (x, s) =
    let y, r = sample proposal in
    let ratio = if Float.(s = zero) then one else r /. s in
    let accept = sample @@ bernoulli @@ Float.min one ratio in
    let next = if accept then (y, r) else (x, s) in
    Yield (next, next)
  in
  let seq = Sequence.unfold_step ~init:(sample proposal) ~f:iterate in
  let seq = Sequence.drop_eagerly seq burn in
  let r = ref seq in
  let sample () =
    match Sequence.next !r with
    | Some (hd, tl) ->
        r := tl;
        fst hd
    | None -> raise Undefined
  in
  sample

let mh_transform ~burn d =
  let open Core.Sequence.Step in
  let proposal = prior_with_score d in
  let iterate (x, s) =
    let y, r = sample proposal in
    let ratio = if Float.(s = zero) then one else r /. s in
    let accept = sample @@ bernoulli @@ Float.min one ratio in
    let next = if accept then (y, r) else (x, s) in
    Yield (next, next)
  in
  let seq = Sequence.unfold_step ~init:(sample proposal) ~f:iterate in
  let seq = Sequence.drop_eagerly seq burn in
  (* burn initial states *)
  let r = ref seq in
  let sample () =
    match Sequence.next !r with
    | Some (hd, tl) ->
        r := tl;
        fst hd
    | None -> raise Undefined
  in
  let* _ = return () in
  return (sample ())

(* could also return a sample function here instead *)
(* Bind(return (), fun v -> s v)
   Bind(return head_value, fun v -> set_tail;return v)
   match Sequence.next !r with
   | Some(hd,tl) -> 
   let* x,_ = hd in
   match Sequence.next tl with
    r := tl;
    (return x)
   | None -> raise Undefined

   let* r' = r in
   match Sequence.next !r' with
   | Some(hd,tl) -> 
   r := tl;
   (return (fst hd))
   | None -> raise Undefined *)

(* match Sequence.next !r with
   | Some (hd, tl) ->
   let* x, _ = hd in
   (* this assignment has to happen inside the bind
     so that successive samples (and calls on the f inside the bind)
     change the mutable state, here the sequence.
  *)
   r := tl;
   return x
   | None -> raise Undefined *)
