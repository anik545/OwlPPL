(** A shallowly embedded DSL for Probabilistic Programming

    This is a library for universal probabilistic programming using distributions as monads. 
    Includes several approximate inference algorithms. 
    Contains utilities to plot distributions and evaluate correctness of inference.
*)

(** {2:core Core } *)

module Dist = Dist
module Primitive = Primitive
module Empirical = Empirical
module Inference = Inference

(** {2:core Extra } *)

module Plot = Plot
module Evaluation = Evaluation
module Helpers = Helpers

(**/**)

module Samples : Empirical.S = Empirical.Discrete

module CSamples = Empirical.ContinuousArr
include Dist
include Inference
include Helpers

(**/**)
