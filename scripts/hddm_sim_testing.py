import kabuki
import hddm
import pandas as pd
import numpy as np

params = {'a': 2, 'v': 1.5, 't': 0.45, 'sa': .2}

sim,sim_params = hddm.generate.gen_rand_data(params)

hddm.wfpt()
model_0 = hddm.HDDM(sim, include = ("sa"))
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



