class DDM(HasTraits):
    """Drift diffusion model"""
    # Paremeters
    z = Range(0, 1., .5)
    sa = Range(0, 1., .0)
    v = Range(-4.,4.,.5)
    sv = Range(0.0,2.,0.0)
    ter = Range(0,2.,.3)
    ster = Range(0,2.,.0)
    a = Range(0.,10.,2.)
    switch = Bool(False)
    t_switch = Range(0,2.,.3)
    v_switch = Range(-20.,20.,1.)
    intra_sv = Range(0.1,10.,1.)
    urgency = Range(.1,10.,1.)

    params = Property(Array, depends_on=['z', 'sa', 'v', 'sv', 'ter', 'ster', 'a']) #, 'switch', 't_switch', 'v_switch', 'intra_sv'])

    # Distributions
    drifts = Property(Tuple, depends_on=['params'])
    rts = Property(Tuple, depends_on=['drifts'])

    params_dict = Property(Dict)

    histo = Property(Array, depends_on=['params'])

    # Total time.
    T = Float(5.0)
    # Time step size
    dt = Float(1e-4)
    # Number of realizations to generate.
    num_samples = Int(5000)
    # Number of drifts to plot
    iter_plot = Int(50)
    # Number of histogram bins
    bins = Int(200)
    view = View('z', 'sa', 'v', 'sv', 'ter', 'ster', 'a', 'num_samples', 'iter_plot') #, 'switch', 't_switch', 'v_switch', 'intra_sv', 'T')

    def _get_params_dict(self):
        d = {'v': self.v, 'sv': self.sv, 'z': self.z, 'sa': self.sa, 't': self.ter, 'st': self.ster, 'a': self.a}
        if self.switch:
            d['v_switch'] = self.v_switch
            d['t_switch'] = self.t_switch
            d['V_switch'] = self.sv
        return d

    @cached_property
    def _get_drifts(self):
        return hddm.generate._gen_rts_from_simulated_drift(self.params_dict, samples=self.iter_plot, dt=self.dt, intra_sv=self.intra_sv)[1]

    @cached_property
    def _get_rts(self):
        if not self.switch:
            # Use faster cdf method
            return hddm.generate.gen_rts(size=self.num_samples, range_=(-self.T, self.T), structured=False, **self.params_dict)
        else:
            # Simulate individual drifts
            return hddm.generate._gen_rts_from_simulated_drift(self.params_dict, samples=self.num_samples, dt=self.dt, intra_sv=self.intra_sv)[0]
            #return hddm.generate.gen_rts(self.params_dict, samples=self.num_samples, range_=(-5, 5), method='drift')

    @cached_property
    def _get_histo(self):
        n, bins = np.histogram(self.rts, bins=2 * self.bins, range=(-self.T, self.T))
        db = np.array(np.diff(bins), float)
        return n / db / (n.sum())

    def _get_params(self):
        return np.array([self.a, self.v, self.ter, self.sa, self.sv, self.ster])

