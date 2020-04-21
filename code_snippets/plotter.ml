Open Ppl.Plot

let () =
  coin_inferred
  |> hist_continuous ~fname
  |> add_pdf ~dist:coin_exact
  |> show
