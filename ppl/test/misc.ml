open Ppl
open Core

let i = infer Models.single_coin (SMC 10)

let e = Primitive.beta 10. 2.

(* let () = Plot.qq_plot ~n:10000 ~fname:"a.png" i e |> Plot.show *)

(* let () = Plot.ecdf_continuous ~n:10000 ~fname:"a.png" i |> add_plot |> Plot.show *)
(* let () =
  Plot.pdf_continuous ~n:1000 ~fname:"a.png" i
  |> Plot.add_exact_pdf ~dist:e ~start:0. ~fin:1.
  |> Plot.show *)

let () =
  Plot.ecdf_continuous ~n:10 ~fname:"a.png" i
  |> Plot.add_exact_cdf ~dist:e |> Plot.show

(* |> Plot.show *)

let _ = Evaluation.kolmogorov_smirnov ~n:500 i e
