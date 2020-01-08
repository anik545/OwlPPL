open Ppl

let infer_strats n = [MH(n);PIMH(n);SMC(n);PC(n)]


let rec repeat_test n test () = 
  if n=0 then () 
  else let () = test() in repeat_test (n-1) test ()

let test_sample_single () =
  Alcotest.(check int) "same int" 5 (sample (return 5))

let test_sample_single_uniform () =
  Alcotest.(check int) "same int" 5 (sample (uniform [5]))

let () = print_endline "asda"
(* Run it *)
let () =
  let open Alcotest in
  run "Dist" [
    "sampling", [
      test_case "sample delta dist" `Quick (repeat_test 100 test_sample_single);
      test_case "sample uniform"    `Quick (repeat_test 100 test_sample_single_uniform);
      test_case "sample uniform"    `Quick (fun () -> print_endline "asda");
    ];
  ]