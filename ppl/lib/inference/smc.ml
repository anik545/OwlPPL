open Common
open Core
open Dist

(* sequential monte carlo - particle filter *)
let rec smc : 'a. int -> 'a dist -> 'a samples dist =
 fun n -> function
  (* at each piece of evidence/data, update each particle by the weight given by c *)
  | Conditional (c, d) ->
      let updated =
        fmap normalise
        @@ condition' (List.sum (module Prob) ~f:snd)
        @@ let* last_particles = smc n d in
           let new_particles =
             List.map (* update particles by weight given by condition *)
               ~f:(fun (x, w) -> (x, c x *. w))
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
          fmap (fun x -> (x, Primitive.pdf d x)) (from_primitive d))
      |> sequence
  (* initialise n particles with the same value and weight *)
  | Return x -> List.init n ~f:(fun _ -> return (x, 1.)) |> sequence

let smc' n d = smc n d >>= categorical

let smcStandard n d = prior' (smc n d)

let smcStandard' n d = prior' (smc' n d)

open Importance

let smcMultiple k n d = fmap flatten (importance k (smc n d))

let smcMultiple' k n d = importance' k (smc' n d)
