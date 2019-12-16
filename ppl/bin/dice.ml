open Ppl
open Core


(* A distribution over the total given a number of dice rolls *)
let rec die = function 
  | 0 -> return 0
  | 1 -> uniform [1;2;3;4;5;6]
  | n -> liftM2 (+) (die 1) (die (n-1))

let k_independent_rolls k = sequence @@ List.init k ~f:(fun _ -> die 1)
let k_independent_n_rolls k n = sequence @@ List.init k ~f:(fun _ -> die n)


let mean1die = sample_mean @@ fmap float_of_int (die 1) (* 3.5 *)
let mean4die = sample_mean @@ fmap float_of_int (die 4) (* 14 *)

let sample_n_indep n = sample @@ k_independent_rolls n


let conditional_die n = condition (fun x -> 1. /. float_of_int x) (die n)

(* normal given x>0  (half normal) *)
let d = condition (fun x -> if Stdlib.(>) x 0. then 0. else -5.) (normal 0. 1.)
let x = mh' 500 d 
let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

(* standard normal *)
let d' = condition (fun _ -> 1.) (normal 0. 1.)
let x' = mh' 500 d'
let m' = sample_mean ~n:1000 x' (* ~ 0 *)