open Helpers
open Core
open Owl_plplot

type handle = Owl_plplot.Plot.handle

type 'a dist = 'a Dist.dist

(* type options = {
  title : string option;
  xlabel : string option;
  ylabel : string option;
} *)

type options = [ `X_label of string | `Y_label of string | `Title of string ]

let one_row_mat xs = Owl.(Mat.of_array xs 1 (Array.length xs))

let one_col_mat xs = Owl.(Mat.of_array xs (Array.length xs) 1)

let set_options h options =
  let rec set = function
    | [] -> ()
    | x :: tl ->
        ( match x with
        | `X_label s -> Plot.set_xlabel h s
        | `Y_label s -> Plot.set_ylabel h s
        | `Title s -> Plot.set_ylabel h s );
        set tl
  in
  set options

let create_with_opts h fname opts =
  let pl = match h with None -> Plot.create ~m:1 ~n:1 fname | Some h -> h in
  set_options pl opts;
  pl

let options_with_defaults options defaults =
  (* for now, just put newer options at the end to override defaults with set is called *)
  defaults @ options

let pdf_continuous ?h ?(n = 5000) ?(fname = "fig.png") ?(options = []) d =
  let options =
    options_with_defaults options
      [ `Title "CDF"; `X_label "X"; `Y_label "Density" ]
  in
  let open Empirical.ContinuousArr in
  let d = from_dist ~n d in
  let pl = create_with_opts h fname options in
  Plot.plot_fun ~h:pl (to_pdf d) (-3.) 3.;
  pl

let hist_dist_continuous ?h ?(n = 5000) ?(fname = "fig.png") ?(options = []) d =
  let options =
    options_with_defaults options
      [ `Title "CDF"; `X_label "X"; `Y_label "Density" ]
  in
  let samples = take_k_samples n d in
  let pl = create_with_opts h fname options in
  Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (one_col_mat samples) 0);
  pl

let ecdf_continuous ?h ?(n = 5000) ?(fname = "fig.png") ?(options = []) d =
  let options =
    options_with_defaults options
      [ `Title "CDF"; `X_label "X"; `Y_label "Cumulative probability" ]
  in
  let samples = take_k_samples n d in
  let pl = create_with_opts h fname options in
  Plot.ecdf ~h:pl (one_col_mat samples);
  pl

let ecdf_discrete ?h ?(n = 5000) ?(fname = "fig.png") ?(options = []) d =
  let samples = take_k_samples n d in
  let options =
    options_with_defaults options
      [ `Title "CDF"; `X_label "X"; `Y_label "Cumulative Probability" ]
  in
  let x, y = Owl_stats.ecdf samples in
  let x = one_row_mat x in
  let y = one_row_mat y in
  let pl = create_with_opts h fname options in
  Plot.stairs ~h:pl x y;
  (* Plot.ecdf ~h:pl (one_col_mat samples); *)
  pl

let hist_dist_discrete ?h ?(n = 5000) ?(fname = "fig.png") ?(options = []) d =
  let open Empirical.Discrete in
  let options =
    options_with_defaults options
      [ `Title "PDF"; `X_label "X"; `Y_label "Probability" ]
  in
  let total = float_of_int n in
  let samples = from_dist d ~n in
  let xs = Array.of_list @@ support samples in
  let ys =
    Array.map ~f:(fun (_, n) -> float_of_int n /. total) @@ to_arr samples
  in
  let pl = create_with_opts h fname options in
  Plot.stem ~h:pl (one_row_mat xs) (one_row_mat ys);
  (* Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0); *)
  pl

let qq_plot ?h ?(n = 1000) ?(fname = "fig.png") ?(options = []) d d' =
  let options =
    options_with_defaults options
      [
        `Title "Q-Q Plot"; `X_label "Exact Distribution"; `Y_label "Sample Data";
      ]
  in

  let samples_d = Array.init n ~f:(fun _ -> Primitive.sample d') in
  let samples_d' = Array.init n ~f:(fun _ -> Dist.sample d) in
  let pl = create_with_opts h fname options in
  Plot.qqplot ~h:pl ~x:(one_row_mat samples_d) (one_row_mat samples_d');
  pl

let pp_plot ?h ?(n = 1000) ?(fname = "fig.png") ?(options = []) d d' =
  let options =
    options_with_defaults options
      [
        `Title "P-P Plot"; `X_label "Exact Distribution"; `Y_label "Sample Data";
      ]
  in

  let cdf_d = Primitive.ppf d' in
  let samples_d' = Array.init n ~f:(fun _ -> Dist.sample d) in

  let pl = create_with_opts h fname options in

  Plot.probplot ~h:pl ~dist:cdf_d (one_row_mat samples_d');
  pl

let add_exact_pdf ?(scale = 1.) ?(start = 0.) ?(fin = 1.) ~dist h =
  Owl_plplot.Plot.(
    plot_fun ~h
      ~spec:[ RGB (0, 0, 255); LineWidth 2. ]
      (fun x -> scale *. Primitive.pdf dist x)
      start fin);
  h

let add_exact_cdf ?(scale = 1.) ?(start = 0.) ?(fin = 1.) ~dist h =
  Owl_plplot.Plot.(
    plot_fun ~h
      ~spec:[ RGB (0, 0, 255); LineWidth 2. ]
      (fun x -> scale *. Primitive.cdf dist x)
      start fin);
  h

let show = Owl_plplot.Plot.output
