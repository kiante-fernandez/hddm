import kabuki
import hddm
import pandas as pd
import numpy as np


include = ["sa", "st", "sv"]
params = hddm.generate.gen_rand_params(include=include) #sometime this generates values that throw an error (particularly when include has sv)
sim,sim_params = hddm.generate.gen_rand_data(params)

hddm.generate.gen_rts(method="cdf", **params).rt.values

rts = np.asarray(sim["rt"])

rt = 1.5
v = params['v']
a = params['a']
t = params['t']
sa = params['sa']
z = 0.5
err = 10 ** (round(np.random.rand() * -10))
st = params['st']
sv = params['sv']

python_wfpt = hddm.wfpt.full_pdf(-rt, v, sv, a, z, sa, t, st, err, n_sa=5)

p = hddm.wfpt.pdf_array(
            rts,
            params["v"],
            params["sv"],
            params["a"],
            params["z"],
            params["sa"],
            params["t"],
            params["st"],
            1e-4,
            logp=True,
        )

summed_logp = np.sum(p)

summed_logp_like = hddm.wfpt.wiener_like(
            rts,
            params["v"],
            params["sv"],
            params["a"],
            params["z"],
            params["sa"],
            params["t"],
            params["st"],
            1e-4,
        )

my_res = hddm.wfpt.full_pdf(
        rt, v=v, sv=sv, a=a, z=z, sa=sa, t=t, st=st, err=err, n_st=5, n_sa=5, use_adaptive=0,)
            
            
model_0 = hddm.HDDM(sim, include = ("sa"))
# find a good starting point which helps with the convergence.
model_0.find_starting_values()
# start drawing 2000 samples and discarding 20 as burn-in (usually you want to have a longer burn-in period)
model_0.sample(2000, burn=20)

stats = model_0.gen_stats()

#notes. could not get to work. Think about simpler example  (rather than calling HDDM) so maybe something in likelihoods.py
#or something

#"""Drift diffusion model"""
    # Paremeters

def get_params_dict(self):
    d = {'v': self.v, 'sv': self.sv, 'z': self.z, 'sa': self.sa, 't': self.ter, 'st': self.ster, 'a': self.a}
    if self.switch:
        d['v_switch'] = self.v_switch
        d['t_switch'] = self.t_switch
        d['V_switch'] = self.sv
        return d


def _get_drifts(self):
    return hddm.generate._gen_rts_from_simulated_drift(self.params_dict, samples=self.iter_plot, dt=self.dt, intra_sv=self.intra_sv)[1]

def _get_rts(self):
    if not self.switch:
            # Use faster cdf method
        return hddm.generate.gen_rts(size=self.num_samples, range_=(-self.T, self.T), structured=False, **self.params_dict)
    else:
            # Simulate individual drifts
        return hddm.generate._gen_rts_from_simulated_drift(self.params_dict, samples=self.num_samples, dt=self.dt, intra_sv=self.intra_sv)[0]
            #return hddm.generate.gen_rts(self.params_dict, samples=self.num_samples, range_=(-5, 5), method='drift')

def _get_histo(self):
    n, bins = np.histogram(self.rts, bins=2 * self.bins, range=(-self.T, self.T))
    db = np.array(np.diff(bins), float)
    return n / db / (n.sum())


def _get_params(self):
    return np.array([self.a, self.v, self.ter, self.sa, self.sv, self.ster])



