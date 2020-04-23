let rec enumerate : type a. a dist -> float -> (a * float) list =
 fun dist multiplier ->
  if multiplier = 0. then []
  else
    match dist with
    | Bind (d, f) ->
        let c = enumerate d multiplier in
        List.concat_map c ~f:(fun (opt, p) -> enumerate (f opt) p)
    | Conditional (c, d) ->
        let c = enumerate d multiplier in
        List.map c ~f:(fun (x, p) -> (x, p *. c x))
    | Primitive p -> (
      match support p with
      | Discrete xs -> List.map xs ~f:(fun x -> (x, multiplier *. pdf p x))
      | Continuous -> raise Undefined )
    | Return x -> [(x, multiplier)]
