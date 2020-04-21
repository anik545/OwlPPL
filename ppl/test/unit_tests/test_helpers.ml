(* Unit tests for the helpers module *)
open Ppl
open Core

let test_normalise_sum_to_1 =
  let open Float in
  QCheck_alcotest.to_alcotest
  @@ QCheck.Test.make ~count:1000 ~name:"test normalisation"
       QCheck.(list (pair int float))
       (fun l ->
         if Stdlib.(l = []) then true
         else
           abs ((List.sum (module Float) ~f:snd @@ normalise l) - 1.)
           < 0.0000001)

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

let test_unduplicate () =
  Alcotest.(check (list (pair int (float 0.0000001))))
    ""
    (unduplicate [ (1, 0.2); (1, 0.4) ])
    [ (1, 0.6) ]

let test_print () =
  let d = from_primitive (Primitive.categorical [ (1, 0.6); (2, 0.4) ]) in
  Alcotest.(check unit) "" (print_exact_exn (module Base.Int) d) ()

let test_memoisation memo () =
  let r = ref 0 in
  let f _ =
    incr r;
    !r + 1
  in
  let f = memo f in
  let x = f 1 in
  let x' = f 1 in
  let y = f 2 in
  Alcotest.(check (list int)) "" [ x; x'; y ] [ 2; 2; 3 ]

let todo () = ()

let w = weighted_dist ~n:10 (categorical [ (1, 0.5) ])

let f = time (fun () -> ())

let test_sample_mean () =
  let i = infer Unit_test_models.single_coin (MH 100) in
  let m = sample_mean ~n:1000 i in
  Alcotest.(check (float 0.5)) "" m 0.83

let tests : unit Alcotest.test list =
  [
    ("normalise", [ test_normalise_sum_to_1 ] @ normalise_tests);
    ( "unduplicate",
      [
        ("no duplicates", `Quick, test_unduplicate);
        ("correct sums", `Quick, todo);
      ] );
    ( "memoise",
      [
        ("for an incrementing function", `Quick, test_memoisation memo);
        ( "for an incrementing function",
          `Quick,
          test_memoisation (memo_no_poly (module Base.Int)) );
      ] );
    ("sampling helpers", [ ("mean", `Quick, test_sample_mean) ]);
    ( "printing",
      [
        ("boolean", `Quick, todo);
        ("int", `Quick, test_print);
        ("float", `Quick, todo);
      ] );
  ]
