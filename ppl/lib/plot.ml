open Helpers

let one_row_mat xs = Owl.(Mat.of_array xs 1 (Array.length xs))

let one_col_mat xs = Owl.(Mat.of_array xs (Array.length xs) 1)

let hist_dist_continuous ?h ?(n = 5000) ?(fname = "fig.png") ?(title = "CDF")
    ?(xlabel = "X") ?(ylabel = "Density") d =
  let open Owl_plplot in
  let samples = take_k_samples n d in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (one_col_mat samples) 0);
  pl

let ecdf_continuous ?h ?(n = 5000) ?(fname = "fig.png") ?(title = "CDF")
    ?(xlabel = "X") ?(ylabel = "Cumulative probability") d =
  let open Owl_plplot in
  let samples = take_k_samples n d in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.ecdf ~h:pl (one_col_mat samples);
  pl

let ecdf_discrete ?h ?(n = 5000) ?(fname = "fig.png") ?(title = "CDF")
    ?(xlabel = "X") ?(ylabel = "Cumulative probability") d =
  let open Owl_plplot in
  let samples = take_k_samples n d in
  (* let map = Empirical.Discrete.(to_norm_arr @@ from_dist ~n d) in *)
  let x, y = Owl_stats.ecdf samples in
  (* let x = one_row_mat (Array.map fst map) in
     let y = one_row_mat (Array.map snd map) in *)
  let x = one_row_mat x in
  let y = one_row_mat y in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.stairs ~h:pl x y;
  (* Plot.ecdf ~h:pl (one_col_mat samples); *)
  pl

let hist_dist_discrete ?h ?(n = 5000) ?(fname = "fig.png") ?(title = "PDF")
    ?(xlabel = "X") ?(ylabel = "Probability") d =
  let open Owl_plplot in
  let open Empirical.Discrete in
  (* let samples = take_k_samples n d in *)
  let total = float_of_int n in
  let samples = from_dist d ~n in
  let xs = Array.of_list @@ support samples in
  let ys =
    Array.map (fun (_, n) -> float_of_int n /. total) @@ to_arr samples
  in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.stem ~h:pl (one_row_mat xs) (one_row_mat ys);
  (* Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0); *)
  pl

let qq_plot ?h ?(n = 1000) ?(fname = "fig.png") ?(title = "")
    ?(xlabel = "Exact distribution") ?(ylabel = "Sample data") d d' =
  let open Owl_plplot in
  (* let Plot.qqplot ~pd:(Primitive.ppf) *)
  let samples_d = Array.init n (fun _ -> Primitive.sample d') in
  let samples_d' = Array.init n (fun _ -> Dist.sample d) in
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in

  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.qqplot ~h:pl ~x:(one_row_mat samples_d) (one_row_mat samples_d');
  pl

let pp_plot ?h ?(n = 1000) ?(fname = "fig.png") ?(title = "")
    ?(xlabel = "Exact distribution") ?(ylabel = "Sample data") d d' =
  let open Owl_plplot in
  (* let Plot.qqplot ~pd:(Primitive.ppf) *)
  let cdf_d = Primitive.ppf d' in
  let samples_d' = Array.init n (fun _ -> Dist.sample d) in

  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in

  Plot.set_title pl title;
  Plot.set_ylabel pl ylabel;
  Plot.set_xlabel pl xlabel;
  Plot.probplot ~h:pl ~dist:cdf_d (one_row_mat samples_d');
  pl

let add_exact_pdf ?(scale = 1.) ~dist h =
  Owl_plplot.Plot.(
    plot_fun ~h
      ~spec:[ RGB (0, 0, 255); LineWidth 2. ]
      (fun x -> scale *. Primitive.pdf dist x)
      0. 1.);
  h

let show = Owl_plplot.Plot.output
