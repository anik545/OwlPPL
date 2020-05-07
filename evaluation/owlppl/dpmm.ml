open Ppl
open Core

let create_stick_break_process concentration =
  let sethuraman_rule = memo (fun _ -> sample (beta 1. concentration)) in
  let rec sample_stick_index k =
    let* keep = bernoulli (sethuraman_rule k) in
    if keep then return k else sample_stick_index (k + 1)
  in
  let stick_breaking_process () = sample_stick_index 0 in
  stick_breaking_process

let dp_mem concentration base =
  let get_val_from_cache_or_sample = memo (fun args _ -> sample (base args)) in
  let get_proc_from_cache =
    memo (fun _ -> create_stick_break_process concentration)
  in
  let f args =
    let idx = sample @@ get_proc_from_cache args () in
    get_val_from_cache_or_sample idx
  in
  f

let input_data = [-20;2;2;2;2;2;9;9;9;9;9;9;9;9;9;9;9;]
  [@ocamlformat "disable"]

let dp_mixture data =
  let h _ =
    let* a = gamma 1. 0.1 in
    let v = 1. /. a in
    return (sample (normal 0. (sqrt (v *. 30.))), sqrt v)
  in
  let obs_param = dp_mem 10.0 h in
  let a1, b1 = obs_param () () in
  List.fold data ~init:(normal a1 b1) ~f:(fun d i ->
      let a, b = obs_param () () in
      observe (float_of_int i) (Primitive.normal a b) d)

open Ppl
open Core

let rec stick bs ats =
  match (bs, ats) with
  | b :: breaks, a :: atoms ->
      let* keep = bernoulli b in
      if keep then return a else stick breaks atoms
  | _ -> failwith "fail"

let breaks _ = memo (fun _ -> sample (beta 1. 1.))

let rec sample_index resid i =
  let* keep = bernoulli (resid i) in
  if keep then return i else sample_index resid (i + 1)

let getBag = memo (fun (_ : int) -> sample @@ sample_index (breaks ()) 0)

let get_stick_index = memo (fun (_ : int) -> getBag)

(* -- | DP mixture example from http://dl.acm.org/citation.cfm?id=2804317 *)
let dpMixture =
  let rec sample_index residual idx =
    let* keep = bernoulli (residual idx) in
    if keep then return idx else sample_index residual (idx + 1)
  in
  let clusters =
    let classgen = sample_index (memo (fun _ -> sample (beta 1. 1.))) 0 in
    (* kinda like lazy lists, take an index  *)
    let* vars = return @@ memo (fun _ -> 10. /. sample (gamma 1. 10.)) in
    let* means = return @@ memo (fun i -> sample @@ normal 0. (vars i)) in
    return (classgen, vars, means)
  in
  let obs = [ 1.; 1.1; 1.2; 3.1; 3.2; 3.15; 3.24 ] in
  (* let n = List.length obs in *)
  let start = fmap (fun x -> (x, [])) clusters in

  let score y (_cluster, var, mean) =
    printf "prob of %f in cluster %d (~N(%f,%f)) is %f\n" y _cluster mean var
      (Primitive.pdf Primitive.(normal mean (sqrt var)) y);
    Primitive.pdf Primitive.(normal mean (sqrt var)) y
  in

  let build d y =
    condition'
      (fun x -> score y (List.hd_exn (snd x)))
      (let* clusters, rest = d in
       let classgen, vars, means = clusters in
       let* cluster = classgen in
       let point = (cluster, vars cluster, means cluster) in
       return (clusters, point :: rest))
  in
  List.fold_left obs ~f:build ~init:start

let cluster_assignments =
  dpMixture
  |> fmap (fun x -> List.rev (List.map ~f:(fun (x, _, _) -> x) (snd x)))

let cluster_parameters =
  dpMixture
  |> fmap (fun x -> List.rev (List.map ~f:(fun (_, m, s) -> (m, s)) (snd x)))

let count_unique lst =
  let unique_set = Stdlib.Hashtbl.create (List.length lst) in
  List.iter ~f:(fun x -> Stdlib.Hashtbl.replace unique_set x ()) lst;
  float_of_int @@ Stdlib.Hashtbl.length unique_set

let dpmm i () = infer cluster_assignments i
