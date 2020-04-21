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
let rec enumerate : type a. a dist -> float -> (a * float) list =
 fun d multiplier ->
  if Float.(multiplier = zero) then []
  else
    match d with
    | Bind (d, f) ->
        let c = enumerate d multiplier in
        List.concat_map c ~f:(fun (opt, p) -> enumerate (f opt) p)
    | Conditional (c, d) ->
        let ch = enumerate d multiplier in
        List.map ch ~f:(fun (x, p) -> (x, p *. Prob.to_float (c x)))
    | Primitive p -> (
        match Primitive.support p with
        | DiscreteFinite xs ->
            List.map xs ~f:(fun x -> (x, multiplier *. Primitive.pdf p x))
        | _ -> raise Undefined )
    | Return x -> [ (x, multiplier) ]

let exact_inference d =
  enumerate d 1. |> unduplicate |> normalise |> categorical

let test =
  let* x = discrete_uniform [ 0; 1 ] in
  let* y = discrete_uniform [ 0; 1 ] in
  condition (x = 1) (return (x + y))

let inferred = exact_inference test
