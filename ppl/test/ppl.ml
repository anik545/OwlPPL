open Ppl

let rec repeat_test n test () = 
  if n=0 then () 
  else let () = test() in repeat_test (n-1) test ()

let test_sample_single () =
  Alcotest.(check int) "same int" 5 (sample (return 5))

let test_sample_single_uniform () =
  Alcotest.(check int) "same int" 5 (sample (uniform [5]))

(* Run it *)
let () =
  let open Alcotest in
  run "Dist" [
    "sampling", [
      test_case "sample delta dist" `Quick (repeat_test 100 test_sample_single);
      test_case "sample uniform"    `Quick (repeat_test 100 test_sample_single_uniform);
    ];
  ]