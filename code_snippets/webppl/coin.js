var coin_model = function (method) {
  var coin = function () {
    var theta = uniform(0.0, 1.0);
    observe(
      Binomial({ n: 10, p: theta }),9
    );
    return theta;
  };
  return Infer(method, coin);
};
