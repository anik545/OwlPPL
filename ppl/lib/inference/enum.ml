include Common
open Core
open Dist

exception Undefined

let fsts = List.map ~f:fst

(* open Dist.Prob *)
(* 'a. 'a dist -> ('a * score list) -> ('a * prob list)  *)
(* produces a list of value,score, not prob (i.e. unnormalised) *)
(* depth-first search *)
(* todo produce a map instead *)
let rec enumerate : type a. a dist -> float -> (a * prob) list =
 fun d multiplier ->
  if Float.(multiplier = zero) then []
  else
    match d with
    | Bind (d, f) ->
        let c = enumerate d multiplier in
        List.concat_map c ~f:(fun (opt, p) ->
            enumerate (f opt) (Prob.to_float p))
    | Conditional (c, d) ->
        let ch = enumerate d multiplier in
        List.map ch ~f:(fun (x, p) -> (x, Prob.(p *. c x)))
    | Primitive p -> (
        match Primitive.support p with
        | DiscreteFinite xs ->
            List.map xs ~f:(fun x ->
                (x, Prob.of_float @@ (multiplier *. Primitive.pdf p x)))
        | _ -> raise Undefined )
    | Return x -> [ (x, Prob.of_float multiplier) ]
    | Independent (d1, d2) ->
        cartesian (enumerate d1 multiplier) (enumerate d2 multiplier)
        |> List.map ~f:(fun ((x1, p1), (x2, p2)) -> ((x1, x2), Prob.(p1 *. p2)))

let exact_inference d =
  enumerate d 1. |> unduplicate |> normalise
  |> List.map ~f:(fun (x, y) -> (x, Dist.Prob.to_float y))
  |> categorical

let test =
  let* x = discrete_uniform [ 0; 1 ] in
  let* y = discrete_uniform [ 0; 1 ] in
  condition (x = 1) (return (x + y))

let test1 =
  let* x = discrete_uniform [ 0; 1 ] and* y = discrete_uniform [ 0; 1 ] in
  condition (x = 1) (return (x + y))

let inferred = exact_inference test

let inferred1 = exact_inference test1
