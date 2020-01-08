open Core
open Dist.GADT_Dist
type prob = float
type 'a samples = ('a * prob) list

let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))

let normalise xs = 
  let norm = List.sum (module Float) ~f:snd xs in
  List.map ~f:(fun (v,p)->(v,p/.norm)) xs

let flatten xss =
  let mul_likelihood xs p = List.map ~f:(fun (x,q) -> (x, p *.q)) xs in
  (* let rec flat_map xss = match xss with
      (xs, p)::xs' -> (mul_likelihood xs p) @ flatten' xs'
     | [] -> []
     in *)
  List.concat_map xss ~f:(fun (xs,p) -> mul_likelihood xs p)
(* flat_map xss *)
