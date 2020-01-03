type prob = float
type 'a dist = ('a * prob) list

let return x = [(x,1.)]
let (>>=) d f = d f