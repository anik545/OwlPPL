var cluster_model = function () {
  var residuals = mem(function (i) {
    return beta(1, 1);
  });
  var sample_index = function (resid, idx) {
    return flip(resid(idx)) ? idx : sample_index(resid, idx + 1);
  };
  var classgen = sample_index(residuals, 0);
  let vars = mem(function (i) {
    10 / gamma(1, 10);
  });
  let means = mem(function (i) {
    normal(0, vars(i));
  });
  var obs = [1, 1.1, 1.2, 3.1, 3.2, 3.15, 3.24];
  map(function (datum) {
    observe();
  }, obs);
};
