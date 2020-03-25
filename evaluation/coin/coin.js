var t0 = console.time("coin");

var coin = function() {
  var theta = uniform(0.0, 1.0);
  observe(Binomial({ n: 10, p: theta }), 9);
  return theta;
};

var inferred = Infer(
  { method: "MCMC", kernel: "MH", samples: 100000, burn: 100 },
  coin
);

var m = 0;
var n = 10000;
for (let x = 0; x < n; x++) {
  m = m + sample(inferred);
}
var mean = m / n;
console.log(mean);
var t1 = console.timeEnd("coin");
var x = sample(inferred);
console.log(x);
