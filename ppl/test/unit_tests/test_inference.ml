(* Unit tests for inference *)
open Ppl
open Core

let extract_exact model =
  match exact_inference model with
  | Primitive xs ->
      let supp =
        match Primitive.support xs with
        | DiscreteFinite xs -> xs
        | _ -> raise Undefined
      in
      List.map supp ~f:(fun x -> (x, (Primitive.pdf xs) x))
  | _ -> []

let test_exact_inference_grass () =
  let grass_model' =
    let* cloudy = bernoulli 0.5 in
    let* rain = bernoulli (if cloudy then 0.8 else 0.2) in
    let* sprinkler = bernoulli (if cloudy then 0.1 else 0.5) in
    let wet_grass = rain || sprinkler in
    condition wet_grass (return rain)
  in
  let p_rain_given_wet_grass = exact_inference @@ grass_model' in
  let x = extract_exact p_rain_given_wet_grass in
  let x = List.sort x ~compare:(fun (a, _) (b, _) -> Bool.compare a b) in
  Alcotest.(check (list (pair bool (float 0.0000001))))
    "check same discrete dist"
    [ (false, 0.2957746478873239); (true, 0.7042253521126761) ]
    x

let test_exact_inference_adding () =
  let model =
    let* x = discrete_uniform [ 0; 1 ] in
    let* y = discrete_uniform [ 0; 1 ] in
    condition (x = 1) (return (x + y))
  in
  let x = extract_exact model in
  let x = List.sort x ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
  Alcotest.(check (list (pair int (float 0.0000001))))
    "check same discrete dist"
    [ (0, 0.); (1, 0.5); (2, 0.5) ]
    x

(* Make sure no conditioning leads to inferring the same as initial distribution *)
let test_exact_inference_no_condition () =
  let dist = bernoulli 0.5 in
  let model =
    let* x = dist in
    return x
  in
  let x = extract_exact model in
  let x = List.sort x ~compare:(fun (a, _) (b, _) -> Bool.compare a b) in
  Alcotest.(check (list (pair bool (float 0.0000001))))
    "check same dist no condition" (extract_exact dist) x

(* TODO: check inference doesnt raise exceptions for each inference method and model *)

open Alcotest

let tests : unit test list =
  [
    ( "inference",
      [
        test_case "adding model" `Quick test_exact_inference_adding;
        test_case "grass model" `Quick test_exact_inference_grass;
        test_case "no conditioning" `Quick test_exact_inference_no_condition;
      ] );
  ]
