open Ppl
open Core

let (+~) = liftM2 (+)
(* A distribution over the total given a number of dice rolls *)
let rec die = function 
  | 0 -> return 0
  | 1 -> uniform [1;2;3;4;5;6]
  | n -> (die 1) +~ (die (n-1))

let k_independent_rolls k = sequence @@ List.init k ~f:(fun _ -> die 1)
let k_independent_n_rolls k n = sequence @@ List.init k ~f:(fun _ -> die n)


let mean1die = sample_mean @@ fmap float_of_int (die 1) (* 3.5 *)
let mean4die = sample_mean @@ fmap float_of_int (die 4) (* 14 *)

let sample_n_indep n = sample @@ k_independent_rolls n

let conditional_die n = condition' (fun x -> 1. /. float_of_int x) (die n)
