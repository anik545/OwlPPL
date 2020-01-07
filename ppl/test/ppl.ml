open Ppl

let test_sample_single () =
  Alcotest.(check int) "same int" 5 (sample (return 5))

let test_sample_single_uniform () =
  Alcotest.(check int) "same int" 5 (sample (uniform [5]))

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
    "sampling", [
      test_case "sample delta dist" `Quick test_sample_single;
      test_case "sample uniform"    `Quick test_sample_single_uniform;
    ];
  ]