open Ppl

(* normal given x>0  (half normal) *)
let d = condition (fun x -> if Stdlib.(>) x 0. then 0. else -5.) (normal 0. 1.)
let x = mh' 500 d 
let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

(* standard normal *)
let d' = condition (fun _ -> 1.) (normal 0. 1.)
let x' = mh' 500 d'
let m' = sample_mean ~n:1000 x' (* ~ 0 *)