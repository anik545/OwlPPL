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
  | Rejection of int * rejection_type
  | RejectionTrans of int * rejection_type
  | Prior
  | Enum
  | Forward (* forward sampling in webppl, no sampling *)
[@@deriving show]

let print_infer_strat = function
    MH _ -> "metropolis hastings"
  | SMC _ -> "particle filter"
  | PC _ -> "particle cascade"
  | PIMH _ -> "particle-independent metropolis-hastings"
  | Importance _ -> "importance"
  | Rejection _ -> "rejection"
  | RejectionTrans _ -> "rejection"
  | Prior -> "prior"
  | Enum -> "enumeration"
  | Forward -> "forward"


let infer dist = function
  | MH(n) -> mh' n dist
  | SMC(n) -> smcStandard' n dist
  | PC(n) -> cascade' n dist
  | PIMH(n) -> pimh' n n dist
  | Importance(n) -> importance' n dist
  | Rejection(n,s) -> rejection s dist ~n
  | RejectionTrans(n,s) -> rejection_transform s dist ~n
  | Enum -> exact_inference dist
  | Prior -> prior' dist
  | Forward -> prior' dist

let infer_sampler dist strat = let new_dist = infer dist strat in fun () -> sample new_dist