open Ppl
open Core

let rec stick (b::breaks) (a::atoms) = 
  let* keep = bernoulli b in
  if keep then return a else stick breaks atoms

let dpMixture =
  let clusters =
    let atoms = [1;2;3] in
    let* breaks = sequence @@ List.init 3 ~f:(fun _ -> beta 1. 1.) in
    let classgen = stick breaks atoms in
    let* vars = sequence @@ List.init 3 ~f:(fun _ -> fmap (fun x-> 1./.x) (gamma 1. 1.)) in
    let* means = mapM (normal 0.) vars in
    return (classgen, vars, means)
  in
  let obs = [1.;1.1;1.2;-1.;-1.5;-2.;0.001;0.01;0.005;0.] in
  let n = List.length obs in
  let start = fmap (fun (x,_,_) -> x ) clusters in

  let score y (_, var, mean) = Primitives.pdf Primitives.(normal mean (sqrt var)) y in

  let build d y = 
    condition' (fun x -> score y (snd (List.hd_exn (x)))) 
      (let* (clusters, rest) = d in
       let (classgen, vars, means) = clusters in
       let* cluster = classgen in
       let point = (cluster, List.nth_exn vars cluster,List.nth_exn means cluster) in
       return (clusters, point::rest))
  in
  let points = List.fold_left obs ~f:build ~init:start in
  fmap (fun x-> 
      List.reverse
        (List.map ~f:(fun (x,_,_) -> x) 
           (snd x)))
    points