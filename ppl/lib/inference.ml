open Dist.GADT_Dist
open Core
module P = Primitive_dists.Primitive_Dists
exception NotImplemented
type prob = float
(* INFERENCE *)


type 'a samples = ('a * prob) list

let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))

let flatten xss =
  let mul_likelihood xs p = List.map ~f:(fun (x,q) -> (x, p *.q)) xs in
  (* let rec flat_map xss = match xss with
      (xs, p)::xs' -> (mul_likelihood xs p) @ flatten' xs'
     | [] -> []
     in *)
  List.concat_map xss ~f:(fun (xs,p) -> mul_likelihood xs p)
(* flat_map xss *)

(* TODO: importance sampling *)

let importance n d = sequence @@ List.init n ~f:(fun _ -> prior d)
let importance' n d = (importance n d) >>= categorical
let normalise xs = 
  let norm = List.sum (module Float) ~f:snd xs in
  List.map ~f:(fun (v,p)->(v,p/.norm)) xs

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
      let* accept = bernoulli @@ Float.min 1. (r /. s) in
      let next = if accept then (y,r) else (x,s) in
      let* rest = iterate ~n:(n-1) next in
      return (next::rest)
  in
  fmap (List.map ~f:fst) (let* x = proposal in iterate x)

let mh' n d = fmap (fun x -> List.nth_exn x (n-1)) (mh n d)

(* sequential monte carlo *)

let rec smc: 'a.int -> 'a dist -> 'a samples dist =
  fun n ->
  function

  | Conditional(c,d) ->
    let updated = fmap normalise @@ 
      condition' (List.sum (module Float) ~f:snd) @@
      let* ps = smc n d in
      let qs = List.map ~f:(fun (x,w) -> (x, (c x) *. w)) ps in
      return qs
    in
    updated >>= resample

  | Bind(d,f) ->
    let* ps = smc n d in
    let (xs, ws) = List.unzip ps in
    let* ys = mapM f xs in
    return (List.zip_exn ys ws)

  | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) d))

let smc' n d = (smc n d) >>= categorical 

let smcStandard n d = prior' (smc n d)
let smcStandard' n d = prior' (smc' n d)

(* TODO: fix importance sampling first *)
(* let smcMultiple k n d = (fmap flatten (importance k (smc n d)))
   let smcMultiple' k n d = (importance' k (smc' n d)) *)

(* particle independent metropolis hastings *)
let pimh n d = mh n (smc n d)
let pimh' k n d = mh' k (smc' n d)


