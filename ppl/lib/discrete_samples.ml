open Dist.GADT_Dist
open Core

module type D_Samples = sig
  type 'a t
  val from_dist: ?n:int -> 'a dist -> 'a t
  val get_num: 'a t -> 'a -> int
  val get_prob: 'a t -> 'a -> float
  (* val print: 'a t -> unit *)
  include Sexpable.S1 with type 'a t := 'a t
end


module Samples: D_Samples = struct 
  type 'a t = ('a, int) Map.Poly.t
  [@@deriving sexp]


  let from_dist ?(n=300) d =
    let rec loop n map =
      if n = 0 then map else
        let s = sample d in 
        let map = Map.Poly.update map s ~f:(fun x -> match x with None -> 1 | Some y -> y + 1) in
        loop (n-1) map
    in
    loop n Map.Poly.empty

  let get_num map x = 
    match Map.Poly.find map x with 
    | Some x -> x
    | None -> 0

  let get_prob map x =
    let total = Map.Poly.fold map ~f:(fun ~key:_ ~data sofar -> sofar + data)  ~init:0 in
    match Map.Poly.find map x with 
    | Some x -> float_of_int x /. float_of_int total
    | None -> 0.

end