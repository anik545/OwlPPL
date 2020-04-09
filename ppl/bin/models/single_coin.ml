open Ppl
open Core

(* https://www.cl.cam.ac.uk/teaching/1819/DataSci/notes0.pdf pg33 *)
let single_coin =
  let pr = continuous_uniform 0. 1. in
  let toss t = condition' (fun p -> Primitive.(pdf @@ binomial 10 p) t) in
  let obs = 9 in
  (* we see x=9 heads *)
  let posterior = toss obs pr in
  (* let posterior' = condition (fun p -> pdf (Binomial(10,p)) obs) (continuous_uniform 0. 1.) in *)
  posterior

(* let coin heads = 
   let* coinweight = continuous_uniform 0. 1. in
   observe heads Primitive.(binomial 10 coinweight)
    (return coinweight) *)

let post_single_coin = mh' 700 @@ single_coin

let mn = sample_mean ~n:100 post_single_coin (* 0.833 *)

let () = Printf.printf "%f\n" mn

(* 
let () =
  let open Owl_plplot in

  let n = 10000 in
  let pl = Plot.create ~m:2 ~n:1 "b1.png" in

  Plot.subplot pl 0 0;
  let pl = hist_dist_continuous ~h:pl ~n post_single_coin in

  Plot.plot_fun ~h:pl (fun x -> (570. /. 4.26) *. (Owl_stats_dist.beta_pdf ~a:10. ~b:2. x)) 0. 1.;

  Plot.subplot pl 1 0;

  Plot.probplot
    ~h:pl
    ~spec:[ MarkerSize 0.25; ]
    ~dist:(Owl_stats_dist.beta_ppf ~a:10. ~b:2.)
    (Owl.(Mat.col (Mat.of_array (take_k_samples n post_single_coin) n 1) 0 ));

  Plot.output pl; *)
