open Ppl
let linreg_model points = 
  let linreg' =
    let* m = normal 0. 2. in
    let* c = normal 0. 2. in
    List.fold 
      points
      ~init:(return (m,c))
      ~f:(fun d (x,y) -> observe y (Primitive.(normal (m*x+c) 1.)) d)

let slope = fmap fst (linreg_model points)
let y_intercept = fmap snd (linreg_model points)