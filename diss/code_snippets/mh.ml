let mh_transform ~burn d =
  let proposal = prior_with_score d in
  let iterate (x, s) =
    let y, r = sample proposal in
    let ratio = if Float.(s = 0.) then 1. else r /. s in
    let accept = sample @@ bernoulli @@ Float.min 1. ratio in
    let next = if accept then (y, r) else (x, s) in
    Yield (return next, next) in
  let seq = Sequence.unfold_step ~init:(sample proposal) ~f:iterate in
  let seq = Sequence.drop_eagerly seq burn in   (* burn initial *)
  let r = ref seq in
  let* _ = return () in  (* to close over the sequence ref *)
  match Sequence.next !r with
  | Some (hd, tl) ->
      let* x, _ = hd in
      r := tl ;
      return x
  | None -> raise Undefined
