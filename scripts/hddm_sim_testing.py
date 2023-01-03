import kabuki
import hddm
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

include = ["sa", "st", "sv"]
params = hddm.generate.gen_rand_params(include=include) #sometime this generates values that throw an error (particularly when include has sv)
params1 = {'sv': 0, 'sa': 0, 'st': 0, 'z': 0.5, 'v': 0.65, 't': 0.23, 'a': 1.5}
params2 = {'sv': 0, 'sa': 0.001, 'st': 0, 'z': 0.5, 'v': 0.65, 't': 0.23, 'a': 1.5}
params3 = {'sv': 0, 'sa': 0.01, 'st': 0, 'z': 0.5, 'v': 0.65, 't': 0.23, 'a': 1.5}
params4 = {'sv': 0, 'sa': 0.1, 'st': 0, 'z': 0.5, 'v': 0.65, 't': 0.23, 'a': 1.5}
params5 = {'sv': 0, 'sa': 1, 'st': 0, 'z': 0.5, 'v': 0.65, 't': 0.23, 'a': 1.5}

sim1,sim_params1 = hddm.generate.gen_rand_data(params1, size = 1000)
sim2,sim_params2 = hddm.generate.gen_rand_data(params2, size = 1000)
sim3,sim_params3 = hddm.generate.gen_rand_data(params3, size = 1000)
sim4,sim_params4 = hddm.generate.gen_rand_data(params4, size = 1000)
sim5,sim_params5 = hddm.generate.gen_rand_data(params5, size = 1000)

sim1['sa'] = 0
sim2['sa'] = 0.001
sim3['sa'] = 0.01
sim4['sa'] = 0.1
sim5['sa'] = 1

sims = pd.concat((sim1,sim2,sim3,sim4,sim5))
sims.to_csv("sa_simulations_test.csv")

sim_list = [sim1, sim2, sim3, sim4, sim5]
fig = plt.figure()
ax = fig.add_subplot(111, xlabel='RT', ylabel='count', title='RT distributions ')
for i in range(5):
    sim = sim_list[i]
    data = hddm.utils.flip_errors(sim)
    # data.rt.hist(bins=50, histtype='step', ax=ax)
    # sns.kdeplot(np.array(data.rt), bw=0.5)
    sns.distplot(data.rt)


plt.show()

sns.kdeplot(np.array(data.rt))
sns.distplot(data.rt)

    

hddm.model_config.model_config["full_ddm"]

model_0 = hddm.HDDM(sim, include = ("sa","sv", "st"))
# find a good starting point which helps with the convergence.
model_0.find_starting_values()
# start drawing 2000 samples and discarding 20 as burn-in (usually you want to have a longer burn-in period)
model_0.sample(10000, burn=5000)

stats = model_0.gen_stats()
stats
sim_params
# params = model_0.optimize('chisquare')
model_0.plot_posteriors(['a','v','sv','sa'])
plt.show()

hddm.plotting.plot_posterior_pair(model_0, save = False, figsize = (10, 10))




# hddm.generate.gen_rts(method="cdf", **params).rt.values

# rts = np.asarray(sim["rt"])

# # rt = 1.5
# v = params['v']
# a = params['a']
# t = params['t']
# sa = params['sa']
# z = 0.5
# err = 10 ** (round(np.random.rand() * -10))
# st = params['st']
# sv = params['sv']

# python_wfpt = hddm.wfpt.full_pdf(-rt, v, sv, a, z, sa, t, st, err, n_sa=5)

# p = hddm.wfpt.pdf_array(rts,
#             params["v"],
#             params["sv"],
#             params["a"],
#             params["z"],
#             params["sa"],
#             params["t"],
#             params["st"],
#             1e-6,
#             logp=True,
#         )

# ar_nan = np.where(np.isinf(p))
# print (ar_nan)
# rts[ar_nan]

# summed_logp = np.sum(p)

# summed_logp_like = hddm.wfpt.wiener_like(
#             rts,
#             params["v"],
#             params["sv"],
#             params["a"],
#             params["z"],
#             params["sa"],
#             params["t"],
#             params["st"],
#             1e-4,
#         )

# my_res = hddm.wfpt.full_pdf(
#         rt, v=v, sv=sv, a=a, z=z, sa=sa, t=t, st=st, err=err, n_st=5, n_sa=5, use_adaptive=0,)
            
            
