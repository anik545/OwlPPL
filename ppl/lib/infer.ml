open Core
open Sigs

module CommonInference = struct 
  type 'a dist = 'a Old_dist.dist
  type prob = float
  type 'a samples = ('a * prob) list

  let (let*) = Old_dist.( let* )
  let (>>=)  = Old_dist.(>>=)
  (* TODO: clean this up - split dist into proper module + make this a functor *)
  let fmap, return, categorical, sequence, bernoulli, mapM, condition, choice
  = 
  Old_dist.(fmap, return, categorical, sequence, bernoulli, mapM, condition, choice)
  exception NotImplemented

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


let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))

let flatten xss =
  let rec f xs p = match xs with 
    | ((x,q)::t) -> (x,p*.q)::(f t p) 
    | [] -> [] in
  let rec flatten' xs = match xs with
      (xs, p)::xs' -> (f xs p) @ flatten' xs'
    | [] -> []
  in 
  flatten' xss

let normalise xs = 
  let norm = List.sum (module Float) ~f:snd xs in
  List.map ~f:(fun (v,p)->(v,p/.norm)) xs

end

module MH: Infer = struct
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
  
  let infer ?(iterations=1000) d = fmap (fun x -> List.nth_exn x (iterations-1)) (mh iterations d)
  
end

module SMC: Infer = struct
  include CommonInference

  let rec smc: 'a.int -> 'a dist -> 'a samples dist =
    fun n ->
    function
    
    | Conditional(c,d) ->
      let updated = fmap normalise @@ 
        condition (List.sum (module Float) ~f:snd) @@
        let* ps = smc n d in
        let qs = List.map ~f:(fun (x,w) -> (x, (c x) *. w)) ps in
        return qs
      in
        updated >>= resample
    
    | Bind(d,f) ->
        let* ps = smc n d in
        let (xs, ws) = List.unzip ps in
        let* ys = mapM f xs in
        return (List.zip_exn ys ws)
  
    | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) d))
  
  let smc' n d = smc n d >>= categorical 
  
  (* let smcStandard n d = prior' (smc n d) *)
  let infer ?(iterations=1000) d = prior' (smc' iterations d)
end

module PC: Infer = struct
  include CommonInference

  let resamplePC ps n = 
    let rec iterate n mean ps iters =
    match ps with
    | [] -> raise NotImplemented
    | (x,w)::ps ->
      let k = float_of_int n in
      let mean' = (k /. (k +. 1.)) *. mean +. (1. /. (k +. 1.)) *. w in
      let r = w /. mean' in
      let flr = Float.round_down r in
      let probLow = flr in
      let clr = Float.round_up r  in
      let probHigh = clr in
      let spawn x w =
        if Float.(r < 1.) then
          choice r (return [(x,mean')]) (return [])
        else
          choice (r -. probLow)
          (return @@ List.init (int_of_float flr) ~f:(fun _ -> x, w /. probLow))
          (return @@ List.init (int_of_float clr) ~f:(fun _ -> x, w /. probHigh))  
      in
      let* children = spawn x w in
      if iters = 0 then return children else
      let* rest = iterate (n+1) mean' ps (iters-1) in
      return (children @ rest)
    in
      iterate 0 0. ps n
  
  let rec cascade:'a.int -> 'a dist -> 'a samples dist = fun n -> function
    | Conditional(c,d) -> 
        let* ps = cascade n d in
        let qs = List.map ~f:(fun (x,w) -> (x,(c x) *. w )) ps in
        resamplePC qs n
    | Bind (d,f) ->
        let* ps = cascade n d in
        let (xs,ws) = List.unzip ps in
        let* ys = mapM f xs in
        return (List.zip_exn ys ws)
  
    | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x -> (x, 1.)) d))
  
  let infer ?(iterations=5000) d = (cascade iterations d) >>= (fun x -> categorical (List.take x iterations))
end
