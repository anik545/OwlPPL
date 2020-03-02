include Common
open Core
open Dist.GADT_Dist.P
open Dist.GADT_Dist

exception Undefined

let fsts = List.map ~f:fst
type score = float

(* 'a. 'a dist -> ('a * score list) -> ('a * prob list)  *)
(* produces a list of value,score, not prob (i.e. unnormalised) *)
(* depth-first search *)
(* todo produce a map instead *)
let rec enumerate: type a.a dist -> score -> ((a * score) list)
  = fun d multiplier ->
    if Float.(multiplier = 0.) then []
    else
      match d with
      | Bind (d,f) ->
        let c = enumerate d multiplier in
        List.concat_map c ~f:(fun (opt, p) -> enumerate (f opt) p)
      | Conditional (c,d) ->
        let ch = enumerate d multiplier in
        List.map ch ~f:(fun (x,p) -> x, p *. (c x))
      | Primitive p ->
        (match support p with
           Discrete xs -> List.map xs ~f:(fun x-> (x,multiplier *. pdf p x))
         | Continuous -> raise Undefined)
      | Return x -> [(x,multiplier)]

let exact_inference d =
  enumerate d 1.
  |> unduplicate
  |> normalise
  |> categorical

let test =
  let* x = discrete_uniform [0;1] in
  let* y  = discrete_uniform [0;1] in
  condition (x=1)
    (return (x + y))

let inferred = exact_inference test
