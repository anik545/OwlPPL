open Helpers

let one_row_mat xs = Owl.(Mat.of_array xs 1 (Array.length xs))

let hist_dist_continuous ?h ?(n = 5000) ?(fname = "fig.png") d =
  let open Owl_plplot in
  let samples = take_k_samples n d in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (one_row_mat samples) 0);
  pl

let hist_dist_discrete ?h ?(n = 5000) ?(fname = "fig.png") d =
  let open Owl_plplot in
  let open Empirical.Discrete in
  (* let samples = take_k_samples n d in *)
  let samples = from_dist d ~n in
  let xs = Array.of_list @@ support samples in
  let ys = Array.map (fun (_, n) -> float_of_int n) @@ to_arr samples in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.stem ~h:pl (one_row_mat xs) (one_row_mat ys);
  (* Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0); *)
  pl

let qq_plot ?h ?(n = 1000) ?(fname = "fig.png") d d' =
  let open Owl_plplot in
  (* let Plot.qqplot ~pd:(Primitive.ppf) *)
  let samples_d = Array.init n (fun _ -> Primitive.sample d') in
  let samples_d' = Array.init n (fun _ -> Dist.sample d) in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.qqplot ~h:pl ~x:(one_row_mat samples_d) (one_row_mat samples_d');
  pl

let prob_plot ?h ?(n = 1000) ?(fname = "fig.png") d d' =
  let open Owl_plplot in
  (* let Plot.qqplot ~pd:(Primitive.ppf) *)
  let cdf_d = Primitive.cdf d' in
  let samples_d' = Array.init n (fun _ -> Dist.sample d) in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.probplot ~h:pl ~dist:cdf_d (one_row_mat samples_d');
  pl
