open Core
open Dist
open Dist.Prob
include Helpers

type 'a samples = ('a * float) list

let resample xs =
  let n = List.length xs in
  (* sample from the distribution specified by the old particles *)
  let old_dist = categorical xs in
  (* generate new particles from th *)
  sequence @@ List.init n ~f:(fun _ -> fmap (fun x -> (x, one)) old_dist)
