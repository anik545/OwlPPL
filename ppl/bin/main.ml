open Ppl
open Owl_plplot

(* normal given x>0  (half normal) *)
let d = condition (fun x -> if Stdlib.(>) x 0. then 0. else 1.) (normal 0. 1.)
let p = prior d
let x = mh' 500 d
let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

(* standard normal *)
let d' = condition (fun _ -> 1.) (normal 0. 1.)
let p' = prior d'
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


let model =
  let* a = uniform [0;1] in
  let* b = uniform [0;1] in
  let* c = uniform [0;1] in
  let d = return ((a,b),c) in
  let d = condition (fun ((a,b),c) -> if a+b+c=3 then 1. else -.Float.infinity) d in
  let* (a,_),_ = d in
  return a

let (+++) f g x = f(g x)
let m = mh' 500 model
let mean = sample_mean ~n:500 @@ fmap float_of_int m;;



let model =
  let factor = condition in
  let (let+) e e1 = Conditional(e, e1) in 

  let* a = uniform [0;1] in
  let* b = uniform [0;1] in
  let* c = uniform [0;1] in
  (* an instance of the >> operator in haskell (sequence) in do notation - used in monad-bayes *)
  let+ _ = condition (if a+b+c = 3 then 0. else -.infinity) in
  return a
in
d
let (+++) f g x = f(g x) 
let m = mh' 500 model
let mean = sample_mean ~n:500 @@ fmap (float_of_int +++ (fst +++ fst)) m;;