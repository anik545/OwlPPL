let dpMixture =
  let rec sample_index residual idx =
    let* keep = bernoulli (residual idx) in
    if keep then return idx else sample_index residual (idx + 1) in
  let clusters =
    let classgen = sample_index (memo (fun _ -> sample (beta 1. 1.))) 0 in
    let* vars = return @@ memo (fun _ -> 10. /. sample (gamma 1. 10.)) in
    let* means = return @@ memo (fun i -> sample @@ normal 0. (vars i)) in
    return (classgen, vars, means) in
  let obs = [1.; 1.1; 1.2; 3.1; 3.2; 3.15; 3.24] in
  let start = fmap (fun x -> (x, [])) clusters in
  let score y (_cluster, var, mean) =
    Primitive.pdf Primitive.(normal mean (sqrt var)) y in
  let build d y =
    condition'
      (fun x -> score y (List.hd_exn (snd x)))
      (let* clusters, rest = d in
       let classgen, vars, means = clusters in
       let* cluster = classgen in
       let point = (cluster, vars cluster, means cluster) in
       return (clusters, point :: rest)) in
  List.fold_left obs ~f:build ~init:start

let cluster_assignments =
  dpMixture
  |> fmap (fun x -> List.rev (List.map ~f:(fun (x, _, _) -> x) (snd x)))
