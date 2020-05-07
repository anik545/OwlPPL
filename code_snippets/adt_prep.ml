type 'a expr = 
    F   of float 
  | B   of bool 
  | Add of float expr * float expr 

(* type checks but wont evaluate *)
Add(F 0.5, B true)
