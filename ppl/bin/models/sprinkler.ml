open Ppl


let grass_model = 
  let* rain = bernoulli 0.3 in
  let* sprinkler = bernoulli 0.5 in
  let* grass_is_wet = bernoulli (if rain then 0.9 else if sprinkler then 0.8 else 0.1) in
  (* let grass_is_wet = bernoulli 0.9 && rain 
                     || bernoulli 0.8 && sprinkler
                     || bernoulli 0.1  *)
  condition grass_is_wet 
    (return (rain))

(* The example used in hansei *)
let flip = bernoulli
let grass_model' =
  let* cloudy    = flip 0.5 in
  let* rain      = flip (if cloudy then 0.8 else 0.2) in
  let* sprinkler = flip (if cloudy then 0.1 else 0.5) in
  (* let* a = flip 0.7 in *)
  let* b = flip 0.9 in
  let* c = flip 0.9 in
  (* let wet_roof  = a && rain in *)
  let wet_grass = b && rain || c && sprinkler in
  condition wet_grass 
    (return rain)
;;

(* let () = match (exact_inference @@ grass_model') with 
    Primitive xs -> 
    let p = P.pdf xs in
    Printf.printf "true:%f false:%f" (p true) (p false)
   | _ -> () *)


(* [(0.707927677329624472, V true); (0.292072322670375473, V false)] *)
