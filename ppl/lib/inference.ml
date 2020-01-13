include Common
include Pc
include Enum
include Importance
include Smc
include Mh
open Dist.GADT_Dist

exception NotImplemented
type infer_strat = 
  | MH of int
  | SMC of int
  | PC of int
  | PIMH of int
  | Importance of int
  | Rejection
  | Prior
  | Enum
  | None (* forward sampling in webppl *)
[@@deriving show]

(* let print_strat = function
   | MH -> 
   | SMC -> 
   | PC ->
   | PIMH 
   | Importance of int
   | Rejection
   | Prior
   | Enum
   | None *)

let infer dist = function
  | MH(n) -> mh' n dist
  | SMC(n) -> smcStandard' n dist
  | PC(n) -> cascade' n dist
  | PIMH(n) -> pimh' n n dist
  | Importance(n) -> importance' n dist
  | Rejection -> raise NotImplemented
  | Prior -> prior' dist
  | Enum -> exact_inference dist
  | None -> dist

let infer_sampler dist strat = let new_dist = infer dist strat in fun () -> sample new_dist