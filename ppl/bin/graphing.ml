open Ppl
open Plot
open Core

let coin_inferred, coin_exact = Single_coin.(post_single_coin, exact_coin)

let root = "/home/anik/Files/work/project/diss/figs/"

let fname = root ^ (Sys.get_argv ()).(1)

let to_draw = (Sys.get_argv ()).(2)

let qq () =
  qq_plot ~fname ~title:"Coin model" ~xlabel:"Exact distribution quantiles"
    ~ylabel:"Sample quantiles" coin_inferred coin_exact
  |> show

let pp () = pp_plot ~fname ~title:"Coin model" coin_inferred coin_exact |> show

let normcdf () = ecdf_continuous ~fname (normal 0. 1.) |> show

let normpdf () = hist_dist_continuous ~fname (normal 0. 1.) |> show

let binompdf () =
  hist_dist_discrete ~n:100 ~fname (fmap float_of_int (binomial 10 0.5)) |> show

let binomcdf () =
  ecdf_discrete ~fname (fmap float_of_int @@ binomial 10 0.5) |> show

let coin_compare () =
  hist_dist_continuous ~fname ~title:"Coin model" coin_inferred
  |> add_pdf ~dist:coin_exact ~scale:72.
  |> show

let () =
  match to_draw with
  | "qq" -> qq ()
  | "pp" -> pp ()
  | "binompdf" -> binompdf ()
  | "binomcdf" -> binomcdf ()
  | "normcdf" -> normcdf ()
  | "normpdf" -> normpdf ()
  | "coincmp" -> coin_compare ()
  | _ -> failwith "not a plot"
