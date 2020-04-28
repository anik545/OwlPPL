open Dist
open Core

(** [unduplicate xs] takes a list of particles and collapses particles with the same value **)
let unduplicate xs =
  let f = Prob.( + ) in
  let map = Map.Poly.of_alist_fold xs ~f ~init:Prob.zero in
  Map.Poly.to_alist map

let normalise xs =
  let norm = Prob.to_float @@ List.sum (module Prob) ~f:snd xs in
  List.map ~f:(fun (v, p) -> (v, Prob.of_float @@ (Prob.to_float p /. norm))) xs

let flatten xss =
  let mul_likelihood xs p = List.map ~f:(fun (x, q) -> (x, Prob.(p *. q))) xs in
  (* let rec flat_map xss = match xss with
      (xs, p)::xs' -> (mul_likelihood xs p) @ flatten' xs'
     | [] -> []
     in *)
  List.concat_map xss ~f:(fun (xs, p) -> mul_likelihood xs p)

(* TODO: make generic - accept an averaging function *)
let sample_mean ?(n = 100000) d =
  let rec loop n d sofar =
    if n = 0 then sofar else loop (n - 1) d (sample d +. sofar)
  in
  loop n d 0. /. float_of_int n

let take_k_samples k d = Core.Array.init k ~f:(fun _ -> sample d)

let sample_variance ?(n = 10000) d = take_k_samples n d |> Owl_stats.var

(* TODO: create a prob_map module *)
let weighted_dist ?(n = 300) (d : 'a dist) : ('a, int) Core.Map.Poly.t =
  let rec loop n map =
    if n = 0 then map
    else
      let s = sample d in
      let map =
        Core.Map.Poly.update map s ~f:(fun x ->
            match x with None -> 1 | Some y -> y + 1)
      in
      loop (n - 1) map
  in
  loop n Core.Map.Poly.empty

let time f =
  Gc.compact ();
  let t = Unix.gettimeofday () in
  let res = f () in
  let t1 = Unix.gettimeofday () in
  let exec_time = t1 -. t in
  Printf.printf "Execution time: %f seconds\n" exec_time;
  (res, exec_time *. 1000.)

(** [memo f] is a memoized version of f. 
    Intended for use with non-deterministic functions so that we fix outputs when called with a given input *)
let memo f =
  let m = Hashtbl.Poly.create ~size:10 () in
  let f' x =
    match Hashtbl.Poly.find m x with
    | Some n -> n
    | None ->
        let el = f x in
        Hashtbl.Poly.add_exn m ~key:x ~data:el;
        el
  in
  f'

let memo_no_poly m f =
  let m = Hashtbl.create m ~size:10 in
  let f' x =
    match Hashtbl.find m x with
    | Some n -> n
    | None ->
        let el = f x in
        Hashtbl.add_exn m ~key:x ~data:el;
        el
  in
  f'

exception Not_discrete

exception Not_primitive

let print_exact_exn (type a) (module S : Base.Stringable.S with type t = a)
    (d : a dist) =
  let open Core in
  match d with
  | Primitive xs ->
      let p = Primitive.pdf xs in
      let s = Primitive.support xs in
      let supp =
        match s with DiscreteFinite s -> s | _ -> raise Not_discrete
      in
      let ps = List.map ~f:(fun x -> (x, p x)) supp in
      List.iter
        ~f:(fun (el, p) -> Printf.printf "%s: %f\n" (S.to_string el) p)
        ps
  | _ -> raise Not_primitive

let print_exact_bool = print_exact_exn (module Base.Bool)

let print_exact_int = print_exact_exn (module Base.Int)

let print_exact_float = print_exact_exn (module Base.Float)
