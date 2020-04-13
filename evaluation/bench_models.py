import sys
import numpy as np
import time
import subprocess

# model, method, data


def js(model, inf="mh", mem=False):
    print(model, inf, "webppl")
    try:
        cmd = ['webppl', model+'.js',
               '--require', 'webppl-timeit', '--', inf]
        if mem:
            cmd = ['time', '-f', '"%M"'] + cmd
        a = subprocess.check_output(
            cmd, stderr=subprocess.STDOUT,
            cwd="/home/anik/Files/work/project/evaluation/webppl")
        return int(float(a.split('\n')[-2].replace('"', '')))
    except:
        return float('inf')


def ocaml(model, inf="mh", mem=False):
    print(model, inf, "owlppl")
    try:
        if ocaml.not_built:
            subprocess.call(['dune', 'build'])
            ocaml.not_built = False
        cmd = ['dune', 'exec', './owlppl_eval.exe', model, inf]
        if mem:
            cmd = ['time', '-f', '"%M"'] + cmd
        a = subprocess.check_output(
            cmd, stderr=subprocess.STDOUT, cwd="/home/anik/Files/work/project/evaluation/owlppl")

        return int(float(a.split('\n')[-2].replace('"', '')))
    except:
        return float('inf')


def py(model, inf='mh', mem=False):
    print(model, inf, "python")
    return 10.


def anglican(model, inf='mh', mem=False):
    print(model, inf, "anglican")
    try:
        cmd = ['lein', 'run', '-m', 'eval_clojure/timeit', model, inf]
        if mem:
            cmd = ['time', '-f', '"%M"'] + cmd
        a = subprocess.check_output(
            cmd, stderr=subprocess.STDOUT,
            cwd="/home/anik/Files/work/project/evaluation/anglican")
        return int(float(a.split('\n')[-2].replace('"', '')))
    except:
        return float('inf')


ocaml.not_built = True

n = 3
models = [
    # 'coin',
    # 'hmm',
    'linreg',
    # 'sprinkler'
]
lang_funcs = [
    ('OwlPPL', ocaml),
    ('WebPPL', js),
    # ('Pyro', py),
    ('Anglican', anglican)
]
infs = ['mh', 'smc', 'rej']

languages = map(lambda (x, y): x, lang_funcs)


def get_stats_for_inf(inf, mem=False):
    return \
        {lang:
         {model: np.array([func(model, inf=inf, mem=mem) for _ in range(n)])
          for model in models}
         for lang, func in lang_funcs
         }


_ = \
    {
        "owl":    {"coin": [], "linreg": []},
        "webppl": {"coin": [], "linreg": []}
    }


def get_stats_for_model(model, mem=False):
    return \
        {lang:
         {inf: np.array([func(model, inf=inf, mem=mem) for _ in range(n)])
          for inf in infs}
         for lang, func in lang_funcs
         }


_ = \
    {
        "owl":    {"mh": [], "smc": []},
        "webppl": {"mh": [], "smc": []}
    }


def write_to_csv(times, fname="times.csv"):

    root_dir = "/home/anik/Files/work/project/diss/data"
    fname = root_dir + '/' + fname
    with open(fname, 'w') as f:
        lines = []
        header = "model,"+",".join(languages) + ",err-"+",err-".join(languages)
        lines.append(header)
        for model in models:
            line = [model]
            for lang in languages:
                line.append(str(np.mean(times[lang][model])))
            for lang in languages:
                interval = (1.96*np.std(times[lang][model]))/np.sqrt(n)
                line.append(str(interval))
            # line = map(
            #     lambda lang: str(times[lang][model]), languages)
            # line = [model] + line
            line = ','.join(line)
            lines.append(line)
        f.write('\n'.join(lines))


def write_to_csv1(times, fname="times.csv"):

    root_dir = "/home/anik/Files/work/project/diss/data"
    fname = root_dir + '/' + fname
    with open(fname, 'w') as f:
        lines = []
        header = "model,"+",".join(languages) + ",err-"+",err-".join(languages)
        lines.append(header)
        for inf in infs:
            line = [inf]
            for lang in languages:
                line.append(str(np.mean(times[lang][inf])))
            for lang in languages:
                interval = (1.96*np.std(times[lang][inf]))/np.sqrt(n)
                line.append(str(interval))
            # line = map(
            #     lambda lang: str(times[lang][model]), languages)
            # line = [model] + line
            line = ','.join(line)
            lines.append(line)
        f.write('\n'.join(lines))


mode = sys.argv[1]

# if mode == "time" or mode == "all":
#     for i in infs:
#         times_inf_by_language = get_stats_for_inf(i, mem=False)
#         write_to_csv(times_inf_by_language, fname="times_" + i + ".csv")

# if mode == "mem" or mode == "all":
#     for i in infs:
#         mems_inf_by_language = get_stats_for_inf(i, mem=True)
#         write_to_csv(mems_inf_by_language, fname="mems_" + i + ".csv")


if mode == "time" or mode == "all":
    for m in models:
        times_inf_by_language = get_stats_for_model(m, mem=False)
        print(times_inf_by_language)
        write_to_csv1(times_inf_by_language, fname="times_" + m + ".csv")

if mode == "mem" or mode == "all":
    for m in models:
        mems_inf_by_language = get_stats_for_model(m, mem=True)
        print(mems_inf_by_language)
        write_to_csv1(mems_inf_by_language, fname="mems_" + m + ".csv")
