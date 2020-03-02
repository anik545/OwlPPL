open Ppl
open Core


let infer_strats n = [MH(n);PIMH(n);SMC(n);PC(n)]

(* let rec repeat_test n test () = 
   if n=0 then () 
   else let () = test() in repeat_test (n-1) test ()

   let test_sample_single () =
   Alcotest.(check int) "same int" 5 (sample (return 5))

   let test_sample_single_uniform () =
   Alcotest.(check int) "same int" 5 (sample (discrete_uniform [5])) *)

let test_exact_inference_grass () = 
  let grass_model' =
    let* cloudy    = bernoulli 0.5 in
    let* rain      = bernoulli (if cloudy then 0.8 else 0.2) in
    let* sprinkler = bernoulli (if cloudy then 0.1 else 0.5) in
    let wet_grass = rain || sprinkler in
    condition wet_grass
      (return rain)
  in
  let p_rain_given_wet_grass = exact_inference @@ grass_model' in

  let x = match (p_rain_given_wet_grass) with
      Primitive xs ->
      let supp = match P.support xs with
          Discrete xs -> xs
        | Continuous -> raise Undefined
      in
      List.map supp ~f:(fun x-> (x,(P.pdf xs) x))
    | _ -> []
  in
  let x = (List.sort x ~compare:(fun (a,_) (b,_) -> Bool.compare a b)) in
  Alcotest.(check (list ((pair (bool) (float 0.0000001))))) "check same discrete dist" 
    ([(false, 0.2957746478873239);(true, 0.7042253521126761);])
    x

let test_exact_inference_adding () =
  let model = 
    let* x = discrete_uniform [0;1] in
    let* y  = discrete_uniform [0;1] in
    condition (x=1)
      (return (x + y))
  in
  let x = match (exact_inference model) with
      Primitive xs ->
      let supp = match P.support xs with
          Discrete xs -> xs
        | Continuous -> raise Undefined
      in
      List.map supp ~f:(fun x-> (x,(P.pdf xs) x))
    | _ -> []
  in
  let x = (List.sort x ~compare:(fun (a,_) (b,_) -> Int.compare a b)) in
  Alcotest.(check (list ((pair int (float 0.0000001))))) "check same discrete dist" 
    ([(0,0.);(1, 0.5);(2, 0.5);])
    x


let test_combining_discrete_distributions () = ()

let test_combining_continuous_distributions () = ()

let qcheck_test_single_value_dists = 
  QCheck.Test.make ~count:1000 ~name:"sample delta dist"
    QCheck.(int)
    (fun x -> sample (return x) = x)

let qcheck_test_single_value_uniform = 
  QCheck.Test.make ~count:1000 ~name:"sample single value uniform"
    QCheck.(list_of_size (fun _ -> 1) int)
    (fun x -> sample (discrete_uniform x) = List.hd_exn x)
(* Alcotest.(check int) "same int" 5 (sample (discrete_uniform [5])) *)

(* Run it *)
let () =
  let open Alcotest in
  run "Dist" [
    "sampling", [
      QCheck_alcotest.to_alcotest qcheck_test_single_value_dists;
      QCheck_alcotest.to_alcotest qcheck_test_single_value_uniform
    ];
    "inference",
    [
      test_case "exact inference" `Quick (test_exact_inference_adding);
      test_case "exact inference" `Quick (test_exact_inference_grass)
    ]
  ]
