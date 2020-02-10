open Ppl

let f () = 
  (* let coin heads = 
     let* coinweight = c_uniform 0. 1. in
     observe heads Primitives.(binomial 10 coinweight)
      (return coinweight)
     in *)
  let coin heads = 
    let posterior = condition' (fun p -> Primitives.(pdf @@ binomial 10 p) heads) (c_uniform 0. 1.) in
    posterior
  in

  let post_single_coin = smcStandard' 1000 @@ coin 9 in
  let mn  = sample_mean ~n:10000 (post_single_coin) (* 0.833 *) in
  mn

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Execution time: %f seconds\n"
    (t1 -. t);
  res

let f' () =
  let x = ref 0. in
  for _ = 0 to 10000 do
    x := !x +. Owl_stats_dist.beta_rvs ~a:10. ~b:2.
  done;
  !x /. 10000.

let mn = time f

let () = Printf.printf "%f" mn