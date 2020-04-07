open Ppl

let grass_model =
  let* rain = bernoulli 0.3 in
  let* sprinkler = bernoulli 0.5 in
  let* grass_is_wet =
    bernoulli (if rain then 0.9 else if sprinkler then 0.8 else 0.1)
  in
  (* let grass_is_wet = bernoulli 0.9 && rain
                     || bernoulli 0.8 && sprinkler
                     || bernoulli 0.1  *)
  condition grass_is_wet (return rain)

(* The example used in hansei *)
let grass_model' =
  let* cloudy = bernoulli 0.5 in
  let* rain = bernoulli (if cloudy then 0.8 else 0.2) in
  let* sprinkler = bernoulli (if cloudy then 0.1 else 0.5) in
  let wet_grass = rain || sprinkler in
  condition wet_grass (return rain)

let p_rain_given_wet_grass = exact_inference @@ grass_model'

let () =
  match exact_inference grass_model' with
  | Primitive xs ->
      let p = P.pdf xs in
      Printf.printf
        "P(rain | grass is wet): %f \nP(not rain | grass is wet): %f\n" (p true)
        (p false)
  | _ -> ()

let () =
  let s = Samples.from_dist (mh' 10000 grass_model') in
  let p = Samples.to_pdf s in
  Printf.printf "P(rain | grass is wet): %f \nP(not rain | grass is wet): %f"
    (p true) (p false)

(* TODO: convert to inline test with ppx_inline_ *)

(* [(0.707927677329624472, V true); (0.292072322670375473, V false)] *)
