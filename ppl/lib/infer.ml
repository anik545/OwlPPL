module type Infer = sig 
  type 'a dist
  type prob = float
  type 'a samples = 'a * prob 
  (* Inference is a procedure which transforms dists*)
  val infer: ?iterations:int -> 'a dist -> 'a dist
end

module type Dist = sig

end


module CommonInference = struct 
  type 'a dist = 'a Dist.dist
  type prob = float

  let rec prior': 'a.'a dist -> 'a dist = function
    Conditional (_,d) -> prior' d
  | Bind (d,f) -> Bind ((prior' d),f)
  | d -> d

let rec prior: 'a.'a dist -> ('a*prob) dist = function
    Conditional (c,d) ->
      let* (x,s) = prior d in
      return (x, s *. (c x))
  | Bind (d,f) ->
      let* (x,s) = prior d in
      let* y = f x in
      return (y, s)
  | d ->
      let* x = d in
      return (x,1.)

type 'a samples = ('a * prob) list

let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))

let flatten xss =
  let rec f xs p = match xs with ((x,q)::t) -> (x,p*.q)::(f t p) | [] -> [] in
  let rec flatten' =
    function
      (xs, p)::xs' -> (f xs p) @ flatten' xs'
    | [] -> []
  in 
  flatten' xss
end

module MH: Infer = struct
  type 'a dist = 'a Dist.dist
  type prob = float
  type 'a samples = 'a * prob
  include CommonInference
  let mh n d =
    let proposal = prior d in
    let rec iterate ?(n=n) (x,s) =
      if n = 0 then return [] else
        let* (y,r) = proposal in
        let* accept = bernoulli @@ Float.min 1. (r /. s) in
        let next = if accept then (y,r) else (x,s) in
        let* rest = iterate ~n:(n-1) next in
        return (next::rest)
    in
    fmap (List.map ~f:fst) (proposal >>= iterate)
  
  let infer ?(n=1000) d = fmap (fun x -> List.nth_exn x (n-1)) (mh n d)
  
end
