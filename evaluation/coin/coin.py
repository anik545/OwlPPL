import time
import pyro
import pyro.optim
import pyro.distributions as dist
import pyro.poutine as poutine
from pyro.infer import MCMC, NUTS, Importance
import torch
import numpy as np


def timer(f):
    t1 = time.time()
    x = f()
    t2 = time.time()
    print(t2-t1, "seconds")
    return x

def f():
    t = 0
    for _ in range(10000):
        t += np.random.beta(10,2)
    return t/10000

def f1():

    def coin():
        weight = pyro.sample("weight", dist.Uniform(0, 1))
        # here we condition on measurement == 9.5
        return pyro.sample("heads", dist.Binomial(10, weight), obs=9)

    # nuts_kernel = NUTS(coin)
    # mcmc = MCMC(nuts_kernel,
    #             num_samples=10_000,
    #             warmup_steps=100,
    #             num_chains=1)
    # mcmc.run(model, data.sigma, data.y)
    # mcmc.summary(prob=0.5)
    posterior = Importance(coin, num_samples=10_000)
    return posterior.run()

if __name__ == "__main__":
    print(timer(f1))
