(* Module to test the evaluation module - hypothesis tests and kl divergence *)

(* open Ppl
   open Core *)

let test_kl_discrete () = failwith "todo"

let test_kl_continuous () = failwith "todo"

let test_ks () = failwith "todo"

let test_chisq () = failwith "todo"

let tests : unit Alcotest.test list =
  [
    ("discrete kl-div", ["", `Quick, test_kl_continuous]);
    ("continuous kl-div", ["", `Quick, test_kl_discrete]);
    ("ks test statistic", ["", `Quick, test_ks]);
    ("chi-sq test statistic", ["", `Quick, test_kl_discrete]);
  ]
