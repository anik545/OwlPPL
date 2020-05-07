include Common
include Pc
include Enum
include Importance
include Smc
include Mh
open Dist

type infer_strat =
  | MH of int
  | SMC of int
  | PC of int
  | PIMH of int
  | Importance of int
  | Rejection of int * rejection_type
  | Prior
  | Enum
  | Forward (* forward sampling in webppl, no sampling *)
[@@deriving show]

let print_infer_strat = function
  | MH _ -> "metropolis hastings"
  | SMC _ -> "particle filter"
  | PC _ -> "particle cascade"
  | PIMH _ -> "particle-independent metropolis-hastings"
  | Importance _ -> "importance"
  | Rejection _ -> "rejection"
  | Prior -> "prior"
  | Enum -> "enumeration"
  | Forward -> "forward"

let print_infer_strat_short = function
  | MH _ -> "mh"
  | SMC _ -> "smc"
  | PC _ -> "pc"
  | PIMH _ -> "pimh"
  | Importance _ -> "importance"
  | Rejection _ -> "rejection"
  | Prior -> "prior"
  | Enum -> "enumeration"
  | Forward -> "forward"

let infer model = function
  | MH n -> mh_transform ~burn:n model
  | SMC n -> smcStandard' n model
  | PC n -> cascade' n model
  | PIMH n -> pimh' n n model
  | Importance n -> importance' n model
  | Rejection (n, s) -> rejection s model ~n
  | Enum -> exact_inference model
  | Prior -> prior' model
  | Forward -> prior' model

let infer_sampler dist strat =
  let new_dist = infer dist strat in
  fun () -> sample new_dist
