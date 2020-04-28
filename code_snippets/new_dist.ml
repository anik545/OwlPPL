(* full definitions omitted *)
let poisson l = (module struct 
  type t = int
  let sample () = ... _
  let pdf k = ... _
  let ppf k = ... _
  let cdf k = ... _
end: PRIM_DIST with type t = int)
