let rec sample : 'a. 'a dist -> 'a = function
  | Return x -> x
  | Bind (d, f) ->
      let y = f (sample d) in
      sample y
  | Primitive d -> Primitive.sample d
  | Conditional (_, _) -> raise Undefined

let rec prior_with_score : 'a. 'a dist -> ('a * prob) dist = function
  | Conditional (c, d) ->
      let* x, s = prior_with_score d in
      return (x, s *. c x)
  | Bind (d, f) ->
      let* x, s = prior_with_score d in
      let* y, s1 = prior_with_score (f x) in
      return (y, s *. s1)
  | Primitive d ->
      let* x = Primitive d in
      return (x, Primitive.pdf d x)
  | Return x -> return (x, 1.)
