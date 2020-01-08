# Features of dsl



define own primitives by extending the Primitive module
only two things needed for a primitive:
 - an exact pdf
 - a way to sample



let* - much like sample in webppl, binds the value to a value from the distribution on the right hand side
```ocaml
let normal
let* a = normal 0. 1. in
(a,a)
```
condition - makes it look like lisp
monad-bayes2015 paper says no conditional on left, 
```
let model1 =
  let condition b d = Conditional((fun _ -> if b then 1. else 0.), d) in
  let* a = uniform [0;1] in
  condition (a=1) 
    (return a)

```
liftM/liftM2 - used to lift standard operators to distributions
```
let (+~) = liftM2 (+)
let a = normal 0. 1. +~ normal 0. 1.
```
can use normal parts of language too, e.g. functions, recursion, if, normal let bindings

can choose