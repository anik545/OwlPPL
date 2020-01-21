open Common
open Core
open Dist.GADT_Dist

(* sequential monte carlo - particle filter *)
let rec smc: 'a.int -> 'a dist -> 'a samples dist =
  fun n ->
  function

  (* At each piece of evidence/data, update each particle by the weight given by c *)
  | Conditional(c,d) ->
    let updated = fmap normalise @@ 
      condition' (List.sum (module Float) ~f:snd) @@
      let* ps = smc n d in
      let qs = List.map ~f:(fun (x,w) -> (x, (c x) *. w)) ps in
      return qs
    in
    updated >>= resample

  | Bind(d,f) ->
    let* ps = smc n d in
    let (xs, ws) = List.unzip ps in
    let* ys = mapM f xs in
    return (List.zip_exn ys ws)

  | Primitive d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, P.pdf d x)) (Primitive d)))
  | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) d))

let smc' n d = (smc n d) >>= categorical

let smcStandard n d = prior' (smc n d)
let smcStandard' n d = prior' (smc' n d)

open Importance
let smcMultiple k n d = (fmap flatten (importance k (smc n d)))
let smcMultiple' k n d = (importance' k (smc' n d))
