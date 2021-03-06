(** A module for empirical distributions generated from samplers  

    Contains a signature as well as two implementations, for continuous and discrete distributions respectively
*)

open Dist
open Core

(* poly version *)
module type S = sig
  type 'a t

  val from_dist : ?n:int -> 'a dist -> 'a t
  (** Create a empirical distribution from a distribution object, using n samples to approximate it *)

  val empty : 'a t
  (** Create an empty distribution *)

  val add_sample : 'a t -> 'a -> 'a t
  (** Add another sample to the distribution *)

  val get_num : 'a t -> 'a -> int
  (** Get the numer of samples with the value *)

  val get_prob : 'a t -> 'a -> float
  (** Get the probability of a particular value *)

  val to_pdf : 'a t -> 'a -> float
  (** Create a pdf function *)

  (* val to_cdf: 'a t -> ('a -> float) *)
  (* val print: 'a t -> unit *)

  val print_map : (module Pretty_printer.S with type t = 'a) -> 'a t -> unit
  (** print the entire distribution *)

  val to_arr : 'a t -> ('a * int) array
  (** Get array of samples *)

  val support : 'a t -> 'a list
  (** Get the set of values for the distribution *)
end

(* TODO: stop using map.poly, slow - take a first class module in the creater functions *)
module Discrete : S = struct
  (** Module to represent discrete empirical distributions, backed by a map *)

  type 'a t = ('a, int) Map.Poly.t [@@deriving sexp]

  let update_elt x =
    let open Option in
    value_map x ~default:1 ~f:(( + ) 1)

  (* match x with 
     | None -> 1 
     | Some y -> y + 1 *)

  (** create empty map *)
  let empty : ('a, int) Map.Poly.t = Map.Poly.empty

  let from_dist ?(n = 300) d =
    let rec loop n map =
      if n = 0 then map
      else
        let s = sample d in
        let map = Map.Poly.update map s ~f:update_elt in
        loop (n - 1) map
    in
    loop n Map.Poly.empty

  let add_sample map x = Map.Poly.update map x ~f:update_elt

  let get_num map x = match Map.Poly.find map x with Some x -> x | None -> 0

  let get_prob map x =
    let total =
      Map.Poly.fold map ~f:(fun ~key:_ ~data sofar -> sofar + data) ~init:0
    in
    match Map.Poly.find map x with
    | Some x -> float_of_int x /. float_of_int total
    | None -> 0.

  let to_pdf map =
    let total =
      Map.Poly.fold map ~f:(fun ~key:_ ~data sofar -> sofar + data) ~init:0
    in
    fun x -> float_of_int (get_num map x) /. float_of_int total

  let support = Map.Poly.keys

  let print (type a) printer ~ppf map =
    let (module P : Pretty_printer.S with type t = a) = printer in
    Map.Poly.iteri map ~f:(fun ~key:x ~data:y ->
        Format.fprintf ppf "%a -> %d\n" P.pp x y)

  let print_map = print ~ppf:Format.std_formatter

  let to_arr samples = Array.of_list @@ Map.Poly.to_alist samples

  (* let to_norm_arr samples =
    let total =
      Map.Poly.fold samples ~f:(fun ~key:_ ~data sofar -> sofar + data) ~init:0
    in
    Array.map
      ~f:(fun (x, s) -> (x, float_of_int s /. float_of_int total))
      (to_arr samples) *)
end

module ContinuousArr = struct
  (** Module to represent continuous empirical distributions, backed by a dynamic array *)

  type 'a t = { samples : float array; n : int; max_length : int }

  (* open Owl_stats *)

  let empty = { samples = Array.create ~len:100 0.; n = 0; max_length = 100 }

  let from_dist ?(n = 300) d =
    let samples = Helpers.take_k_samples n d in
    { samples; n; max_length = n }

  let add_sample { samples; n; max_length } x =
    let n = n + 1 in
    if n > max_length then (
      let new_length = max_length * 2 in
      let new_arr = Array.create ~len:new_length 0. in
      Array.blit ~src:samples ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:(n - 1);
      new_arr.(n) <- x;
      { samples = new_arr; n; max_length = new_length } )
    else (
      samples.(n - 1) <- x;
      { samples; n; max_length } )

  let to_cdf_arr d = Owl_stats.ecdf d.samples

  let to_pdf_arr d =
    let cdf = Owl_stats.ecdf d.samples in
    let vals = fst cdf in
    let f = snd cdf in
    let arr = Array.create ~len:(Array.length (fst cdf)) 0. in
    for i = 0 to Array.length arr - 2 do
      (* gradients at each point *)
      arr.(i) <- (f.(i + 1) -. f.(i)) /. (vals.(i + 1) -. vals.(i))
    done;
    (vals, arr)

  (* interpolated ecdf *)
  let to_cdf d =
    let e = Owl_stats.ecdf d.samples in
    let x' = fst e in
    let f = snd e in
    let cdf x =
      match Float.(Array.findi ~f:(fun _ el -> el >= x) x') with
      | Some (idx, _) ->
          let open Int in
          if idx = 0 then 0.
          else
            let lowx = x'.(idx - 1) in
            let lowy = f.(idx - 1) in
            let highx = x'.(idx) in
            let highy = f.(idx) in
            Float.(lowy + ((highy - lowy) * ((x - lowx) / (highx - lowx))))
      | None -> 1.
    in
    cdf

  let to_pdf d =
    let open Float in
    let cdf = to_cdf d in
    let max = Option.value ~default:1. @@ Array.max_elt ~compare d.samples in
    let min = Option.value ~default:0. @@ Array.min_elt ~compare d.samples in
    let delta = (max -. min) /. 20. in
    let pdf x = (cdf (x +. delta) -. cdf x) /. delta in
    pdf

  let values d = Array.sub d.samples ~pos:0 ~len:(d.n - 2)

  let print d =
    printf "Samples: ";
    Array.iter ~f:(printf "%f,") d.samples;
    printf "\nn: %d\n" d.n

  (* 
  open Ppl
  open Core
  open Empirical.ContinuousArr
  let test = 
    let open Owl_plplot in
    let emp = from_dist (normal 0. 1.) in
    Plot.plot_fun (to_pdf emp) (-.3.) 3.
 *)
  (* let print (type a) printer ~ppf map = 
         let (module P: Pretty_printer.S with type t = a) = printer in
         failwith "-"

         let print_map = failwith "-"
         let to_arr samples = failwith "-"
         let to_norm_arr samples = failwith "-" *)
end
