type _ expr = 
    F   : float -> float expr
  | B   : bool -> bool expr
  | Add : float expr * float expr 
          -> float expr
(* correctly doesn't type check *)
Add(F 0.5, B true)
