open Ppl
open Core

let create_stick_break_process concentration = 
  let sethuraman_rule = memo (fun _ -> sample (beta 1. concentration)) in
  let rec sample_stick_index k = 
    let* keep = bernoulli (sethuraman_rule k) in
    if keep then return k
    else sample_stick_index (k+1) 
  in
  let stick_breaking_process () = sample_stick_index 0 in
  stick_breaking_process

let dp_mem concentration base = 
  let get_val_from_cache_or_sample = memo (fun args _ -> base args) in
  let get_proc_from_cache = memo (fun _ -> create_stick_break_process concentration) in
  let f args = 
    let idx = get_proc_from_cache args () in
    get_val_from_cache_or_sample idx
  in f

let input_data = [-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3]

(* let dp_mixture data = 
   let h _ = 
    let v = 1. /. (sample (gamma 1. 0.1)) in
    [sample (normal 0. (sqrt (v*.30.))); (sqrt v)]

   in
   let obs_param = dp_mem 10.0 h in
   () *)


(* open Ppl
   open Core

   let rec stick (b::breaks) (a::atoms) = 
   let* keep = bernoulli b in
   if keep then return a else stick breaks atoms

   let breaks _ = memo (fun _ -> sample (beta 1. 1.))

   let rec sample_index resid i = 
   let* keep = bernoulli (resid i) in
   if keep then return i else
    sample_index resid (i+1)

   let getBag = memo (fun _ -> sample @@ sample_index (breaks ()) 0)

   let get_stick_index = memo (fun _ -> getBag)

   let dpMixture =
   let clusters =
    let classgen = sample_index (breaks ()) 0 in
    let* vars = sequence @@ List.init 3 ~f:(fun _ -> fmap (fun x-> 1./.x) (gamma 1. 1.)) in
    let* means = mapM (normal 0.) vars in
    return (classgen, vars, means)
   in
   let obs = [1.;1.1;1.2;-1.;-1.5;-2.;0.001;0.01;0.005;0.] in
   let n = List.length obs in
   let start = fmap (fun (x,_,_) -> x ) clusters in

   let score y (_, var, mean) = Primitive.pdf Primitive.(normal mean (sqrt var)) y in

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
*)
let dpMixture _ _ = () 