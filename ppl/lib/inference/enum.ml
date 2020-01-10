include Common
open Core
open Dist.GADT_Dist.P
open Dist.GADT_Dist

exception Undefined

let fsts = List.map ~f:fst
type score = float

let unduplicate xs = 
  let map = Core.Map.Poly.of_alist_fold xs ~f:(+.) ~init:0. in
  Core.Map.Poly.to_alist map

(* 'a. 'a dist -> ('a * score list) -> ('a * prob list)  *)
(* produces a list of value,score, not prob (i.e. unnormalised) *)
(* todo produce a map instead *)
let rec enumerate: type a.a dist -> score -> ((a * score)list)
  = fun d multiplier ->
    if Float.(multiplier = 0.) then [] 
    else 
      match d with
      | Bind (d,f) ->
        let c = enumerate d multiplier in
        let mul_snd q = List.map ~f:(fun (x,p) -> (x, p*.q)) in
        List.concat_map c ~f:(fun (opt, p) -> enumerate (f opt) multiplier |> (mul_snd p))

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
  let* x = uniform [0;1] in
  let* y  = uniform [0;1] in
  condition (x=1)
    (return (x + y))

let inferred = exact_inference test
