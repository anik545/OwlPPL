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

let hist_dist_continuous ?h ?(n=5000) ?(fname="fig.jpg") d = 
  let open Owl_plplot in 
  let samples = take_k_samples n d in

  let pl = match h with 
    | None -> Plot.create ~m:1 ~n:1 fname 
    | Some h -> h 
  in
  Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0);
  pl

let hist_dist_discrete ?h ?(n=5000) ?(fname="fig.jpg") d = 
  let open Owl_plplot in
  let open Samples.DiscreteSamples in 
  (* let samples = take_k_samples n d in *)
  let samples = from_dist d ~n in
  let xs = Array.of_list @@ support samples in
  let ys = Array.map (fun (_,n) -> float_of_int n) @@ to_arr samples in
  let pl = match h with 
    | None -> Plot.create ~m:1 ~n:1 fname 
    | Some h -> h
  in
  Plot.stem ~h: pl Owl.(Mat.of_array xs 1 (Array.length xs)) Owl.(Mat.of_array ys 1 (Array.length ys));
  (* Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0); *)
  pl

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
