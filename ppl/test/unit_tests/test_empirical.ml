open Ppl
open Core

let todo () = ()

let test_disc =
  Samples.(add_sample (add_sample (add_sample (add_sample empty 1) 2) 3) 1)

let test_cont = CSamples.from_dist (normal 0. 1.)

let d_pdf () =
  let f = Samples.to_pdf test_disc in
  let g = Samples.get_prob test_disc in
  Alcotest.(check (list (float 0.000001)))
    "check pdf function is correct"
    (List.map ~f [ 1; 2; 3; 4 ] @ List.map ~f:g [ 1; 2; 3; 4 ])
    ([ 0.5; 0.25; 0.25; 0. ] @ [ 0.5; 0.25; 0.25; 0. ])

let d_supp () =
  let l = Samples.support test_disc in
  Alcotest.(check (list int)) "check support function is correct" l [ 1; 2; 3 ]

let c_pdf () =
  let f = CSamples.to_pdf test_cont in
  Alcotest.(check pass) "check pdf function is correct" (f 0.) 0.3

let c_cdf () =
  let g = CSamples.to_cdf test_cont in
  Alcotest.(check pass) "check cdf function is correct" (g 0.) 1.

let c_more_samples () =
  let test_cont = CSamples.add_sample test_cont 10. in
  let test_cont = CSamples.add_sample test_cont 20. in
  let test_cont = CSamples.add_sample test_cont 30. in
  (* let g = CSamples.to_cdf test_cont in *)
  let v = CSamples.values test_cont in
  let v', _ = CSamples.to_pdf_arr test_cont in
  let open Float in
  Alcotest.(check bool)
    "check array out contains values" true
    (Array.mem ~equal v 10. && Array.mem ~equal v' 10.)

let tests : unit Alcotest.test list =
  [
    ( "discrete",
      [
        ("from dist", `Quick, todo);
        ("pdf", `Quick, d_pdf);
        (* ("cdf", `Quick, todo); *)
        ("support", `Quick, d_supp);
      ] );
    ( "continuous",
      [
        ("from dist", `Quick, todo);
        ("pdf", `Quick, c_pdf);
        ("cdf", `Quick, c_cdf);
        ("support", `Quick, todo);
        (* ("adding samples", `Quick, c_more_samples); *)
      ] );
  ]
