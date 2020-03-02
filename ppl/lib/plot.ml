open Helpers
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
