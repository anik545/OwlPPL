open Ppl
open Owl_plplot

(* normal given x>0  (half normal) *)
let d = condition (fun x -> if Stdlib.(>) x 0. then 0. else 1.) (normal 0. 1.)
let x = mh' 500 d 
let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

(* standard normal *)
let d' = condition (fun _ -> 1.) (normal 0. 1.)
let x' = mh' 500 d'
let m' = sample_mean ~n:1000 x' (* ~ 0 *)


(* let h = Owl_stats.(histogram (`N 10) l)
let h' = Owl_stats.(histogram (`N 10) l') *)

let () = 
  let n = 10000 in 
  let pl = Plot.create ~m:2 ~n:1 "fig.jpg" in
  
  Plot.subplot pl 0 0;
  let pl = hist_dist ~h:pl ~n x in

  Plot.subplot pl 1 0;
  let pl = hist_dist ~h:pl ~n x' in

  Plot.output pl
