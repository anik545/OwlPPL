import time
import pymc3 as pm
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
    m = pm.Model()
    with m as model:
        theta = pm.Uniform('theta', 0, 1)
        obs = pm.Binomial('obs', n=10, p=theta, observed=9)
        step = pm.Metropolis()
        trace = pm.sample(1, chains=10000, step=step, tune=100)
        
    return np.mean(trace['theta'])

if __name__ == "__main__":
    print(timer(f1))
