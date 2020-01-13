open Core

(* How would an user add a new distribution? *)
module type Primitives = sig
  type 'a primitive
  val binomial : int -> float -> int primitive
  val normal : float -> float -> float primitive
  val categorical : ('a * float) list -> 'a primitive
  val discrete_uniform : 'a list -> 'a primitive
  val beta : float -> float -> float primitive
  val gamma : float -> float -> float primitive
  val continuous_uniform : float -> float -> float primitive


  val sample : 'a primitive -> 'a
  val pdf : 'a primitive -> 'a -> float
  val logpdf : 'a primitive -> 'a -> float
  type 'a support = Discrete of 'a list | Continuous

  val support: 'a primitive -> 'a support
end

(* type 'a prim_record = {sample: unit -> 'a; pdf: 'a -> float} *)
module Primitive_Dists: Primitives = struct
  (* exception Undefined *)

  type 'a support = Discrete of 'a list | Continuous

  type 'a primitive = {sample: unit -> 'a; pdf: 'a -> float; support: 'a support}

  let binomial n p = {
    sample = (fun () -> Owl_stats_dist.binomial_rvs ~n ~p);
    pdf = Owl_stats_dist.binomial_pdf ~n ~p;
    support = Continuous
  }

  let normal mu sigma = {
    sample=(fun() -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma);
    pdf= Owl_stats_dist.gaussian_pdf ~mu ~sigma;
    support = Continuous
  }

  let categorical xs = 
    let ps = Array.of_list_map ~f:snd xs in
    let xs_arr = Array.of_list_map ~f:fst xs in
    {sample=
       (fun()->
          xs_arr.(Owl_stats.categorical_rvs ps)
          (* let xs = List.sort xs 
              ~compare:(fun a b ->  if Float.(snd a -. snd b < 0.) then 1 else -1) in
             let r = Owl_base_stats.uniform_rvs ~a:0.0 ~b:1. in
             let rec loop p_left remaining = match p_left with
              (v,p)::xs -> 
              if Float.(remaining < p) || Stdlib.(xs = []) 
              then v 
              else loop xs (remaining -. p)
             | [] -> raise Undefined
             in
             loop xs r *)
       );
     pdf=(fun x -> 
         let rec lookup l = function 
           | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
           | [] -> 0. (* not found *)
         in
         lookup x xs);
     support = Discrete (List.map ~f:fst xs)
    }

  let discrete_uniform xs = {
    sample=(fun()->(Owl_base_stats.sample (Array.of_list xs) 1).(0));
    pdf=(fun _ -> 1. /. (float_of_int (List.length xs)));
    support = Discrete xs
  }
  let beta a b = {
    sample=(fun()->Owl_stats_dist.beta_rvs ~a ~b);
    pdf=Owl_stats_dist.beta_pdf ~a ~b;
    support = Continuous
  }
  let gamma shape scale = {
    sample=(fun()->Owl_stats_dist.gamma_rvs ~shape ~scale);
    pdf=Owl_stats_dist.gamma_pdf ~shape ~scale;
    support = Continuous
  }
  let continuous_uniform a b = {
    sample=(fun()->Owl_stats_dist.uniform_rvs ~a ~b);
    pdf=Owl_stats_dist.uniform_pdf ~a ~b;
    support = Continuous
  }

  let sample s = s.sample ()
  let pdf s x = s.pdf x
  let logpdf s x = log (s.pdf x)
  let support s = s.support
end

module Primitive_Dists_GADT: Primitives = struct 
  type prob = float
  exception Undefined
  exception NotImplemented
  type _ primitive = 
      Normal: float * float -> float primitive 
    | Uniform: 'a list -> 'a primitive 
    | Categorical: ('a * prob) list -> 'a primitive
    | Beta: float * float -> float primitive
    | Binomial: int * float -> int primitive
    | C_Uniform: float * float -> float primitive
    | Gamma: float * float -> float primitive

  (* type a is a locally abstract type *)
  let sample: type a.a primitive -> a = function
      Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma
    | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
    | Categorical xs ->
      let xs = List.sort xs ~compare:(fun a b ->  if Float.(snd a -. snd b < 0.) then 1 else -1) in
      let r = Owl_base_stats.uniform_rvs ~a:0.0 ~b:1. in
      let rec loop p_left remaining = match p_left with
          (v,p)::xs -> if Float.(remaining < p) || Stdlib.(xs = []) then v else loop xs (remaining -. p)
        | [] -> raise Undefined
      in
      loop xs r
    | Beta (a, b) -> Owl_stats_dist.beta_rvs ~a ~b
    | Binomial (n, p) -> Owl_stats_dist.binomial_rvs ~n ~p
    | C_Uniform (a,b) -> Owl_stats_dist.uniform_rvs ~a ~b
    | Gamma(_,_) -> raise NotImplemented

  let pdf: type a.a primitive -> a -> float = function
    (* cont *)
    | Normal(mu,sigma) -> Owl_stats_dist.gaussian_pdf ~mu ~sigma
    | Beta (a, b) -> Owl_stats_dist.beta_pdf ~a ~b
    | C_Uniform (a, b) -> Owl_stats_dist.uniform_pdf ~a ~b

    (* discrete *)
    | Uniform us -> fun _ -> 1. /. (float_of_int (List.length us))
    | Categorical cs -> fun x -> 
      let rec lookup l = function 
        | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
        | [] -> 0. (* not found *)
      in
      lookup x cs
    | Binomial (n,p) -> Owl_stats_dist.binomial_pdf ~n ~p
    | Gamma(_,_) -> raise NotImplemented

  let logpdf s x = log (pdf s x)

  let binomial n p  = Binomial(n,p)
  let normal mu sigma = Normal(mu,sigma)
  let categorical xs = Categorical(xs)
  let discrete_uniform xs = Uniform(xs)
  let beta a b = Beta(a,b)
  let gamma shape scale = Gamma(shape,scale)
  let continuous_uniform a b = C_Uniform(a,b)

  type 'a support = Discrete of 'a list | Continuous

  let support _ = raise NotImplemented
end

