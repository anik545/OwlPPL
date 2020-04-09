open Ppl.Dist
open Core

let qcheck_test_single_value_dists =
  QCheck.Test.make ~count:1000 ~name:"sample delta dist"
    QCheck.(int)
    (fun x -> sample (return x) = x)

let qcheck_test_single_value_uniform =
  QCheck.Test.make ~count:1000 ~name:"sample single value uniform"
    QCheck.(list_of_size (fun _ -> 1) int)
    (fun x -> sample (discrete_uniform x) = List.hd_exn x)

(* TODO: Add tests for monad laws *)
let test_right_id_law () = ()

let test_left_id_law () = ()

let test_assoc_law () = ()

open Alcotest

let tests : unit test list =
  [
    ( "sampling",
      [
        QCheck_alcotest.to_alcotest qcheck_test_single_value_dists;
        QCheck_alcotest.to_alcotest qcheck_test_single_value_uniform;
      ] );
    ("monad laws", [ test_case "assoc law" `Quick test_assoc_law ]);
  ]
