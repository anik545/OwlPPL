include Dist.GADT_Dist
include Inference
include Helpers

module Plot : module type of Plot = Plot

module Primitives = Prim_dist
module Evaluation = Evaluation

module Samples : module type of Empirical.Discrete = Empirical.Discrete
