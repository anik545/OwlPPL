open Option

let add_options x y = 
  (product x y)
    >>= (fun (a,b) -> 
      Some (a+b)))
