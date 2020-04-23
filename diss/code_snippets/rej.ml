let rejection ?(threshold = 0.) dist =
  (* repeat until a sample is accepted *)
  let rec repeat () =
    let* x, s = prior_with_score dist in
    if Float.(s > threshold) then return (x, s) else repeat () in
  repeat ()
