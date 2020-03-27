import numpy as np
import time
import subprocess


def js(model, inf="mh"):
    try:
        a = subprocess.check_output(
            ['webppl', model+'/'+model+'.js',
                '--require', 'webppl-timeit', '--', inf],
            cwd="/home/anik/Files/work/project/evaluation")
        return int(float(a.split('\n')[-2]))
    except:
        return float('inf')


def ocaml(model, inf="mh"):
    try:
        if ocaml.not_built:
            subprocess.call(['dune', 'build'])
            ocaml.not_built = False
        a = subprocess.check_output(
            ['dune', 'exec', model+'/'+model+'.exe', inf], cwd="/home/anik/Files/work/project/evaluation")
        return int(float(a.split('\n')[-2]))
    except:
        return float('inf')


def py(model, inf='mh'):
    return 10.

ocaml.not_built = True

n = 1
models = ['coin', 'hmm', 'linreg']
lang_funcs = [('OwlPPL', ocaml), ('WebPPL', js), ('Pyro', py)]
languages = map(lambda (x, y): x, lang_funcs)
infs = ['mh', 'smc', 'rej']


def get_times_for_inf(inf):
    return \
        {lang:
         {model: np.mean(np.array([func(model, inf=inf) for _ in range(n)]))
          for model in models}
         for lang, func in lang_funcs
         }


def write_to_csv(times, fname="times.csv"):

    root_dir = "/home/anik/Files/work/project/diss/data"
    fname = root_dir + '/' + fname
    with open(fname, 'w') as f:
        lines = []
        header = "model,"+",".join(languages)
        lines.append(header)
        for model in models:
            line = map(
                lambda lang: str(times[lang][model]), languages)
            line = [model] + line
            line = ','.join(line)
            lines.append(line)
        f.write('\n'.join(lines))


for i in infs:
    times_inf_by_language = get_times_for_inf(i)
    write_to_csv(times_inf_by_language, fname="times_" + i + ".csv")
