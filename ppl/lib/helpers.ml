open Dist.GADT_Dist

(* TODO: make generic - accept an averaging function *)
let sample_mean ?(n=100000) d = 
  let rec loop n d sofar = 
    if n = 0 
    then sofar
    else loop (n-1) d ((sample d) +. sofar)
  in
  (loop n d 0.) /. float_of_int n

let take_k_samples k d = Core.Array.init k ~f:(fun _ -> sample d)


let undup xs = 
  let map = Core.Map.Poly.of_alist_fold xs ~f:(+.) ~init:0. in
  map


(* TODO: create a prob_map module *)
let weighted_dist ?(n=300) (d: 'a dist) : ('a, int) Core.Map.Poly.t =
  let rec loop n map =
    if n = 0 then map else
      let s = sample d in 
      let map = Core.Map.Poly.update map s ~f:(fun x -> match x with None -> 1 | Some y -> y + 1) in
      loop (n-1) map
  in
  loop n Core.Map.Poly.empty

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Execution time: %f seconds\n"
    (t1 -. t);
  res
;;