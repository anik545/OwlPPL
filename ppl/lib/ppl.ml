(** DSL for Probabilistic Programming

    Universal PPL in OCaml
*)

(** {2:submods Submodules } *)

module Plot = Plot
module Primitive = Primitive
module Evaluation = Evaluation
module Empirical = Empirical
module Inference = Inference
module Dist = Dist
module Helpers = Helpers

module Samples : Empirical.S = Empirical.Discrete

module CSamples = Empirical.ContinuousArr

(* */** *)
include Dist
include Inference
include Helpers

(* */** *)
