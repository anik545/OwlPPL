open Common
open Core
open Dist

(* sequential monte carlo - particle filter *)
let rec smc : type a. int -> a dist -> a samples dist =
 fun n -> function
  (* at each piece of evidence/data, update each particle by the weight given by c *)
  | Conditional (c, d) ->
      let updated =
        fmap normalise
        @@ condition' (fun l ->
               Prob.to_float @@ (List.sum (module Prob) ~f:snd) l)
        @@ let* last_particles = smc n d in
           let new_particles =
             List.map (* update particles by weight given by condition *)
               ~f:(fun (x, w) -> Prob.(x, c x *. w))
               last_particles
           in
           return new_particles
      in
      updated >>= resample
  (* apply function to each particle, no resampling *)
  | Bind (d, f) ->
      let* particles = smc n d in
      mapM
        (fun (x, weight) ->
          let* y = f x in
          return (y, weight))
        particles
  (* initialise n particles wih weights from the pdf *)
  | Primitive d ->
      List.init n ~f:(fun _ ->
          fmap
            (fun x -> (x, Prob.of_float @@ Primitive.pdf d x))
            (from_primitive d))
      |> sequence
  (* initialise n particles with the same value and weight *)
  | Return x -> List.init n ~f:(fun _ -> return (x, Prob.one)) |> sequence
  | Independent (d1, d2) ->
      let* particles1 = smc n d1 in
      let* particles2 = smc n d2 in
      cartesian particles1 particles2
      |> List.map ~f:(fun ((x1, p1), (x2, p2)) -> ((x1, x2), Prob.(p1 *. p2)))
      |> return

let smc' n d =
  let* l = smc n d in
  let l = List.map ~f:(fun (x, y) -> (x, Dist.Prob.to_float y)) l in
  categorical l

let smcStandard n d = prior' (smc n d)

let smcStandard' n d = prior' (smc' n d)

open Importance

let smcMultiple k n d = fmap flatten (importance k (smc n d))

let smcMultiple' k n d = importance' k (smc' n d)
