open Ppl
open Core

let obs =
  [
    0.9;
    0.8;
    0.7;
    0.;
    -0.025;
    5.;
    2.;
    0.1;
    0.;
    0.13;
    0.45;
    6.;
    0.2;
    0.3;
    -1.;
    -1.;
  ]

let transition = function
  | -1 -> categorical @@ List.zip_exn [ -1; 0; 1 ] [ 0.1; 0.4; 0.5 ]
  | 0 -> categorical @@ List.zip_exn [ -1; 0; 1 ] [ 0.2; 0.6; 0.2 ]
  | 1 -> categorical @@ List.zip_exn [ -1; 0; 1 ] [ 0.15; 0.7; 0.15 ]
  | _ -> raise Undefined

let emission x = Primitive.normal (float_of_int x) 1.

let initial = discrete_uniform [ [ -1 ]; [ 0 ]; [ 1 ] ]

let hmm_continuous = Hmm.hmm_general transition emission [ 0. ] initial
