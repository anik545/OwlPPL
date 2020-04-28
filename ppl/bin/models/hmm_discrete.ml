open Ppl
open Core

let transition = function
  | true -> categorical @@ List.zip_exn [ true; false ] [ 0.7; 0.3 ]
  | false -> categorical @@ List.zip_exn [ true; false ] [ 0.3; 0.7 ]

let emission = function
  | true -> Primitive.categorical @@ List.zip_exn [ true; false ] [ 0.9; 0.1 ]
  | false -> Primitive.categorical @@ List.zip_exn [ true; false ] [ 0.1; 0.9 ]

let initial = return [ true ]

let obs = [ true; true; true ]

let hmm_continuous = Hmm.hmm_general transition emission obs initial
