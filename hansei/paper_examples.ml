(* Examples in the paper *)

open Ptypes
open ProbM;;

(* The extended grass model *)
let flip = fun p -> dist [(p, true); (1.-.p, false)];;

let grass_model = fun () ->
  let cloudy    = flip 0.5 in
  let rain      = flip (if cloudy then 0.8 else 0.2) in
  let sprinkler = flip (if cloudy then 0.1 else 0.5) in
  let wet_roof  = flip 0.7 && rain in
  let wet_grass = flip 0.9 && rain || flip 0.9 && sprinkler in
  if wet_grass then rain else fail ()
;;

let t1exact = exact_reify grass_model;;
let [(0.4581, V true); (0.188999999999999974, V false)]
    = t1exact;;

let normalize l = 
  let total = List.fold_left (fun acc (p,_) -> p +. acc) 0.0 l in
  List.map (fun (p,v) -> (p /. total,v)) l;;

let t1exact' = normalize t1exact;;
let [(0.707927677329624472, V true); (0.292072322670375473, V false)]
    = t1exact';;


(* The grass model with memoization *)

let grass_model_memo = fun () ->
  let cloudy    = memo (fun () -> flip 0.5) in
  let rain      = memo (fun () -> flip (if cloudy () then 0.8 else 0.2)) in
  let sprinkler = memo (fun () -> flip (if cloudy () then 0.1 else 0.5)) in
  let wet_roof  = memo (fun () -> flip 0.7 && rain ()) in
  let wet_grass = memo (fun () -> flip 0.9 && rain () || flip 0.9 && sprinkler ()) in
  if wet_grass () then rain () else fail ()
;;


let t2exact = exact_reify grass_model_memo;;
let [(0.458100000000000063, V true); (0.189000000000000029, V false)]
    = t2exact;;

(* Nested inference *)

let prob_of (v_test : 'a) (pv : 'a pV) =
  try fst (List.find (fun (p, V v) -> v = v_test) pv) with Not_found -> 0.;;

(* 
  Choose a coin that is either fair or completely biased
  for |true|, with equal probability.  Let $p$ be the probability that 
  flipping the coin yields |true|.  What is the probability that $p$ is at
  least 0.3?  It is $1$, of course, because both 0.5 and 1 are at least 0.3.
  In the code below, the |at_least 0.3 true| tests if a given 
  probability table assigns probability at least 0.3 to the outcome |true|.
*)

let at_least prob v pv = prob_of v pv >= prob;;

let [(1., V true)] =
exact_reify (fun () ->
  let biased = flip 0.5 in
  let coin = fun () -> flip 0.5 || biased in
  at_least 0.3 true (exact_reify coin));;

(* Illustrating inference about inference.
   Suppose we choose a coin as before, then estimate $p$ by flipping the 
   coin twice and dividing the count of |true| by~2 (that is, by using
   rejection sampling with count 2). What is the probability that our
   \emph{estimate} is at least 0.3?  It is 7/8, because the only way for
   us to estimate below 0.3 is to choose a fair coin and get |false| from
   it twice.
*)

let [(0.875, V true); (0.125, V false)] =
exact_reify (fun () ->
  let biased = flip 0.5 in
  let coin = fun () -> flip 0.5 || biased in
  at_least 0.3 true (sample_rejection dist_selector 2 coin));;

(* We can just as well use memoization *)
let [(0.875, V true); (0.125, V false)] =
exact_reify (fun () ->
  let biased = letlazy_nesting (fun () -> flip 0.5) in
  let coin = fun () -> flip 0.5 || biased () in
  at_least 0.3 true (sample_rejection dist_selector 2 coin));;

