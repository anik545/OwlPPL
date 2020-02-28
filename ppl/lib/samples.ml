open Dist.GADT_Dist
open Core

module type Samples = sig
  type 'a t
  val from_dist: ?n:int -> 'a dist -> 'a t
  val empty: 'a t
  val add_sample: 'a t -> 'a -> 'a t
  val get_num: 'a t -> 'a -> int
  val get_prob: 'a t -> 'a -> float
  val to_pdf: 'a t -> ('a -> float)
  (* val print: 'a t -> unit *)
  val print_map: (module Pretty_printer.S with type t = 'a) -> 'a t -> unit
  include Sexpable.S1 with type 'a t := 'a t
  val to_arr : 'a t -> ('a * int) array
  val to_norm_arr : 'a t -> ('a * float) array
  val support : 'a t -> 'a list
end


module DiscreteSamples: Samples with type 'a t = ('a, int) Map.Poly.t = struct 
  type 'a t = ('a, int) Map.Poly.t
  [@@deriving sexp]

  let update_elt x = 
    let open Option in
    value_map x ~default:1 ~f:((+) 1) 
  (* match x with 
     | None -> 1 
     | Some y -> y + 1 *)

  let empty: ('a, int) Map.Poly.t = Map.Poly.empty

  let from_dist ?(n=300) d =
    let rec loop n map =
      if n = 0 then map else
        let s = sample d in 
        let map = Map.Poly.update map s ~f:update_elt in
        loop (n-1) map
    in
    loop n Map.Poly.empty

  let add_sample map x =
    Map.Poly.update map x ~f:update_elt

  let get_num map x = 
    match Map.Poly.find map x with 
    | Some x -> x
    | None -> 0

  let get_prob map x =
    let total = Map.Poly.fold map ~f:(fun ~key:_ ~data sofar -> sofar + data)  ~init:0 in
    match Map.Poly.find map x with 
    | Some x -> float_of_int x /. float_of_int total
    | None -> 0.

  let to_pdf map = 
    let total = Map.Poly.fold map ~f:(fun ~key:_ ~data sofar -> sofar + data)  ~init:0 in
    fun x ->
      float_of_int (get_num map x) /. float_of_int total

  let support = Map.Poly.keys

  let print (type a) printer ~ppf map = 
    let (module P: Pretty_printer.S with type t = a) = printer in
    Map.Poly.iteri 
      map 
      ~f:(fun ~key:x ~data:y -> Format.fprintf ppf "%a -> %d\n" P.pp x y)

  let print_map = print ~ppf:Format.std_formatter

  let to_arr samples = Array.of_list @@ Map.Poly.to_alist samples 

  let to_norm_arr samples = 
    let total = Map.Poly.fold samples ~f:(fun ~key:_ ~data sofar -> sofar + data)  ~init:0 in
    Array.map ~f:(fun (x,s) -> (x,float_of_int s /. float_of_int total)) (to_arr samples)

end

(* TODO: do one for continuous dists as well *)

(* module WeightedSamples: Samples = struct
   type 'a t = ('a, float) Map.Poly.t
   [@@deriving sexp]

   end *)