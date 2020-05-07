let sprinkler_model =
  let* cloudy   = bernoulli .8 in
  let* rain     = bernoulli 
   (if cloudy then .8 else .1) in
  let* sprinkler = bernoulli 
   (if cloudy then .1 else .5) in
  let* wet_grass = bernoulli 
    (match rain,sprinkler with
         true,true -> .99
       | true,false -> .9
       | false,true -> .9
       | false,false -> 0.) in
  condition wet_grass
    (return rain)