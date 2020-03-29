type 'a dist = ('a * float) list

let unduplicate = _
let bind d f = 
  let mult_snd p = List.map ~f:(fun (a,x) -> (a,x*.p)) in
  List.concat_map ~f:(fun (x,p) -> mult_snd (f x) p) d

let return x = [(x,1.)]
