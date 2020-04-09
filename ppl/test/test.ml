open Core

(* namespace tests by module *)
let ns_tests ts ns =
  List.map ts ~f:(fun (name, test_case) -> (ns ^ ":" ^ name, test_case))

let ns_run suite_name test_lists =
  Alcotest.run suite_name
  @@ List.concat (List.map test_lists ~f:(fun (x, y) -> ns_tests y x))

let () =
  ns_run "OwlPPL"
    [
      ("dist", Test_dist.tests);
      ("empirical", Test_empirical.tests);
      ("helpers", Test_helpers.tests);
      ("inference", Test_inference.tests);
      ("evaluation", Test_evaluation.tests);
      ("plot", Test_plot.tests);
      ("primitives", Test_primitive.tests);
    ]
