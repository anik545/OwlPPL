module type PRIM_DIST = sig
  type t
  val sample: unit -> t
  val pdf: t -> float
  val cdf: t -> float 
end
