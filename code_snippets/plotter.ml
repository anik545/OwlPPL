Open Ppl.Plot

let coin_compare () =
  hist_dist_continuous ~fname coin_inferred 
  |> add_pdf ~dist:coin_exact 
  |> show
