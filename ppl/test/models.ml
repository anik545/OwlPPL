open Ppl
open Core

let list_gen =
  memo (fun n ->
      List.init n ~f:(fun n ->
          let n = float_of_int n in
          (n, (n *. 2.) +. 1.)))

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

(* ---------- SPRINKLER ---------- *)
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

(* ---------- LINEAR REGRESSION ---------- *)

(* let linreg obs =
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
  let posterior = points obs linear in
  posterior *)

let linreg obs =
  let linear =
    let* a = normal 0. 2. in
    let* b = normal 0. 2. in
    let* c = gamma 1. 1. in
    return (a, b, c)
  in
  let open Float in
  let summer l a b c =
    List.sum
      ~f:(fun (x, y) -> Primitive.(pdf @@ normal ((a * x) + b) c) y)
      (module Float)
      l
  in
  let points d obs = condition' (fun (a, b, c) -> summer obs a b c) d in
  let posterior = points linear obs in
  posterior

(* y=2x+1 *)
let obs =
  List.init 20 ~f:(fun i ->
      let i = float_of_int i in
      (i, (i *. 2.) +. 1.))

let linreg_m = fmap fst3 (linreg obs)

let linreg_m_exact = Primitive.normal 2. 1.

(* ---------- HMM ---------- *)

let transition = function
  | true -> categorical @@ List.zip_exn [ true; false ] [ 0.7; 0.3 ]
  | false -> categorical @@ List.zip_exn [ true; false ] [ 0.3; 0.7 ]

let emission = function
  | true -> Primitive.categorical @@ List.zip_exn [ true; false ] [ 0.9; 0.1 ]
  | false -> Primitive.categorical @@ List.zip_exn [ true; false ] [ 0.1; 0.9 ]

let initial = return [ true ]

let obs = [ true; true; true ]

let hmm_general transition emission observed_values start =
  let score y x = Primitive.pdf (emission x) y in
  let expand d y =
    condition' (fun l -> score y (List.hd_exn l))
    @@ let* rest = d in
       let* x = transition (List.hd_exn rest) in
       return (x :: rest)
  in
  let states = List.fold_left ~f:expand ~init:start observed_values in
  liftM List.rev states

let hmm = hmm_general transition emission obs initial

let hmm_1 = fmap (fun l -> List.nth_exn l 1) hmm

let hmm_1_exact =
  Primitive.categorical [ (true, 0.92528736); (false, 0.07471264) ]

let hmm_2 = fmap (fun l -> List.nth_exn l 2) hmm

let hmm_3 = fmap (fun l -> List.nth_exn l 3) hmm

let hmm_exact =
  (* true; false *)
  [
    [ 0.92528736; 0.07471264 ];
    [ 0.68390805; 0.31609195 ];
    [ 0.84482759; 0.15517241 ];
  ]

(* (* let rec stick bs ats =
  match (bs, ats) with
  | b :: breaks, a :: atoms ->
      let* keep = bernoulli b in
      if keep then return a else stick breaks atoms *)
  | _ -> failwith "fail" *)

(* let breaks _ = memo (fun _ -> sample (beta 1. 1.)) *)

let rec sample_index resid i =
  let* keep = bernoulli (resid i) in
  if keep then return i else sample_index resid (i + 1)

(* let getBag = memo (fun (_ : int) -> sample @@ sample_index (breaks ()) 0)

let get_stick_index = memo (fun (_ : int) -> getBag) *)

(* -- | DP mixture example from http://dl.acm.org/citation.cfm?id=2804317 *)
let dpMixture obs =
  let rec sample_index residual idx =
    let* keep = bernoulli (residual idx) in
    if keep then return idx else sample_index residual (idx + 1)
  in
  let clusters =
    let classgen = sample_index (memo (fun _ -> sample (beta 1. 1.))) 0 in
    (* kinda like lazy lists, take an index  *)
    let* vars = return @@ memo (fun _ -> 10. /. sample (gamma 1. 10.)) in
    let* means = return @@ memo (fun i -> sample @@ normal 0. (vars i)) in
    return (classgen, vars, means)
  in
  (* let n = List.length obs in *)
  let start = fmap (fun x -> (x, [])) clusters in

  let score y (_cluster, var, mean) =
    Primitive.pdf Primitive.(normal mean (sqrt var)) y
  in

  let build d y =
    condition'
      (fun x -> score y (List.hd_exn (snd x)))
      (let* clusters, rest = d in
       let classgen, vars, means = clusters in
       let* cluster = classgen in
       let point = (cluster, vars cluster, means cluster) in
       return (clusters, point :: rest))
  in
  List.fold_left obs ~f:build ~init:start

let cluster_assignments obs =
  dpMixture obs
  |> fmap (fun x -> List.rev (List.map ~f:(fun (x, _, _) -> x) (snd x)))

let cluster_parameters obs =
  dpMixture obs
  |> fmap (fun x -> List.rev (List.map ~f:(fun (_, m, s) -> (m, s)) (snd x)))

let count_unique lst =
  let unique_set = Stdlib.Hashtbl.create (List.length lst) in
  List.iter ~f:(fun x -> Stdlib.Hashtbl.replace unique_set x ()) lst;
  float_of_int @@ Stdlib.Hashtbl.length unique_set
