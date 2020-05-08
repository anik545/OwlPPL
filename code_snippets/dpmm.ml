let dpMixture =
  let rec sample_index residual idx =
    let* keep = bernoulli (residual idx) in
    if keep then return idx else sample_index residual (idx + 1) in
  let clusters =
    let class_dist = sample_index (memo (fun _ -> sample (beta 1. 1.))) 0 in
    let* variances = return @@ memo (fun _ -> 10. /. sample (gamma 1. 10.)) in
    let* means = return @@ memo (fun i -> sample @@ normal 0. (variances i)) in
    return (class_dist, variances, means) in
  let obs = [1.; 1.1; 1.2; 3.1; 3.2; 3.15; 3.24] in
  let init = fmap (fun x -> (x, [])) clusters in
  let score y (_cluster, var, mean) =
    Primitive.pdf Primitive.(normal mean (sqrt var)) y in
  let add_point d y =
    condition'
      (fun x -> score y (List.hd_exn (snd x)))
      (let* clusters, rest = d in
       let class_dist, variances, means = clusters in
       let* cluster = class_dist in
       let point = (cluster, variances cluster, means cluster) in
       return (clusters, point :: rest)) in
  List.fold_left obs ~f:add_point ~init

let cluster_assignments =
  dpMixture
  |> fmap (fun x -> List.rev (List.map ~f:(fun (x, _, _) -> x) (snd x)))
