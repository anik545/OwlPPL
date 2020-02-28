module type ExactDist = sig
  type 'a t
  val pdf: 'a t -> float
  val cdf: 'a t -> float
  val logpdf: 'a t -> float
  val logcdf: 'a t -> float
end

(* A distribution that can have inference applied *)
module type ApproximateDist = sig
  type 'a t
end


(* A distribution that can be directly sampled from (i.e. result of inference, or a primitive) *)
module type SampleDist = sig
  type 'a t
  val sample: 'a t -> 'a
  (* generate a sampler that can be called *)
  val sampler: 'a t -> (unit -> 'a)
end
