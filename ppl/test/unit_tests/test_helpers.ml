(* Unit tests for the helpers module *)
open Ppl
open Core

let test_normalise_sum_to_1 =
  let open Float in
  QCheck.Test.make ~count:1000 ~name:"test normalisation"
    QCheck.(list (pair int float))
    (fun l ->
      if Stdlib.(l = []) then true
      else
        abs ((List.sum (module Float) ~f:snd @@ normalise l) - 1.) < 0.0000001)

let test_norms (desc, input, expected) () =
  Alcotest.(check (list (pair char (float 0.00001))))
    desc (normalise input) expected

let normalise_io =
  (* input, expected output *)
  [
    ("empty lists", [], []);
    ("sums to less than one", [ ('a', 0.5) ], [ ('a', 1.) ]);
    ("sums to greater than one", [ ('a', 1.5) ], [ ('a', 1.) ]);
    (* ("sums to 0", [ ('a', 0.) ], ['a', -.nan]); *)
    ("correct weights", [ ('a', 0.2); ('b', 0.6) ], [ ('a', 0.25); ('b', 0.75) ]);
  ]

let normalise_tests =
  List.map normalise_io ~f:(fun t -> (fst3 t, `Quick, test_norms t))

let test_unduplicate = unduplicate

let todo () = ()

open QCheck_alcotest

let tests : unit Alcotest.test list =
  [
    ("normalise", [ to_alcotest test_normalise_sum_to_1 ] @ normalise_tests);
    ( "unduplicate",
      [ ("random testing", `Quick, todo); ("empty list", `Quick, todo) ] );
  ]
