var colors = ["blue", "green", "red"];
var observedMarbles = [
  { name: "obs1", draw: "red" },
  { name: "obs2", draw: "blue" },
  { name: "obs3", draw: "red" },
  { name: "obs4", draw: "blue" },
  { name: "obs5", draw: "red" },
  { name: "obs6", draw: "blue" }
];

var results = Infer(
  { method: "MCMC", samples: 200, lag: 100 },

  function() {
    var phi = dirichlet(ones([3, 1]));
    var alpha = 0.1;
    var prototype = T.mul(phi, alpha);
    var makeBag = mem(function(bag) {
      var colorProbs = dirichlet(prototype);
      return Categorical({ vs: colors, ps: colorProbs });
    });

    //a prior over an infinite set of bags:
    var residuals = mem(function(i) {
      beta(1, 1);
    });
    var mySampleDiscrete = function(resid, i) {
      return flip(resid(i)) ? i : mySampleDiscrete(resid, i + 1);
    };
    var getBag = mem(function(obs) {
      return mySampleDiscrete(residuals, 0);
    });

    mapData({ data: observedMarbles }, function(d) {
      observe(makeBag(getBag(d.name)), d.draw);
    });

    return {
      samebag12: getBag("obs1") == getBag("obs2"),
      samebag13: getBag("obs1") == getBag("obs3")
    };
  }
);

viz.marginals(results);
