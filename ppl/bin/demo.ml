open Ppl
open Core

(* let inferred = infer Sprinkler.grass_model'' (Importance 100)

let s = Samples.from_dist ~n:1000 inferred

let () = Samples.print_map (module Base.Bool) s *)

(* let () = print_exact_bool inferred *)

(* let inferred = infer Single_coin.single_coin (MH 100)

let mean = sample_mean ~n:100_000 inferred

let () = printf "%f" mean

let () =
  Plot.pdf_continuous ~fname:"a.png" inferred
  |> Plot.add_exact_pdf ~dist:(Primitive.beta 10. 2.)
  |> Plot.show *)

let inferred = infer Linreg.linreg'' (MH 1000)

let mean = sample_mean ~n:100_000 (fmap fst inferred)

let () = Plot.pdf_continuous ~fname:"a.png" (fmap fst inferred) |> Plot.show
