open Ppl.Dist
open Core
open Ppl.Inference

let qcheck_test_single_value_dists =
  QCheck.Test.make ~count:1000 ~name:"sampling single value distribution "
    QCheck.(int)
    (fun x -> sample (return x) = x)

let qcheck_test_single_value_uniform =
  QCheck.Test.make ~count:1000
    ~name:"sampling single value uniform distribution"
    QCheck.(list_of_size (fun _ -> 1) int)
    (fun x -> sample (discrete_uniform x) = List.hd_exn x)

let test_liftm () =
  let open PplOps in
  Alcotest.(check pass) "" (sample @@ (normal 0. 1. +.~ normal 0. 1.)) 0.

(* TODO: Add tests for monad laws *)
let test_right_id_law () = ()

let test_left_id_law () = ()

let test_assoc_law () = ()

let test_samples () = Alcotest.(check int) "" (sample (return 1)) 1

let test_sample_n () =
  Alcotest.(check pass)
    ""
    (sample_n 100 (infer Unit_test_models.single_coin (MH 100)))
    [||]

let test_sample_score () =
  Alcotest.(check pass)
    ""
    (sample_with_score Unit_test_models.grass_model)
    (true, 0.2)

open Alcotest

let tests : unit test list =
  [
    ( "sampling",
      [
        QCheck_alcotest.to_alcotest qcheck_test_single_value_dists;
        QCheck_alcotest.to_alcotest qcheck_test_single_value_uniform;
        ("sample", `Quick, test_samples);
        ("sample with score", `Quick, test_sample_score);
        ("take n samples", `Quick, test_sample_n);
      ] );
    ("monad laws", [ test_case "assoc law" `Quick test_assoc_law ]);
  ]
