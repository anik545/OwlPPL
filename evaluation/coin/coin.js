var t0 = console.time("coin");
var coin = function() {
  var theta = uniform(0.0, 1.0);
  observe(Binomial({ n: 10, p: theta }), 9);
  return theta;
};
var inferred = Infer(
  { method: "MCMC", kernel: "MH", samples: 10000, burn: 100 },
  coin
);

var t1 = console.timeEnd("coin");
inferred.m;
