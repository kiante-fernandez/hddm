.. index:: Tutorials
.. _chap_tutorial_hddm_regression:

Stimulus coding with HDDMRegression
###################################

Note: This tutorial is more advanced. If you are just starting you might want
to head to the :ref:`basic tutorial <chap_basic_tutorial>` instead.

In some situations it is useful to fix the magnitude of parameters
across stimulus types while also forcing them to have different
directions. For example, an independent variable could influence both
the drift rate ``v`` and the response bias ``z``. A specific example is an
experiment on face-house discrimination with different difficulty
levels, where the drift-rate is smaller when the task is more
difficult and where the bias to responding house is larger when the
task is more difficult.  One way to analyze the effect of difficulty
on drift rate and bias in such an experiment is to estimate one drift
rate ``v`` for each level, and a response bias ``z`` such that the bias for
houses-stimuli is ``z`` and the bias for face stimuli is ``1-z`` (``z = .5``
for unbiased decisions in ``HDDM``).

The following example describes how to generate simulated data for
such an experiment, how to set up the analysis with ``HDDMRegression``,
and compares true parameter values with those estimated with
``HDDMRegression``.

Model Recovery Test for HDDMRegression
**************************************

The test is performed with simulated data for an experiment with one
independent variable with three levels (e.g. three levels of
difficulty) which influence both drift rate ``v`` and bias ``z``. Responses
are "accuracy coded", i.e. correct responses are coded ``1`` and incorrect
responses ``0``. Further, stimulus coding of the parameter ``z`` is
implemented. "stimulus coding" of ``z`` means that we want to fit a model
in which the magnitude of the bias is the same for the two stimuli,
but its direction "depends on" the presented stimulus (e.g. faces or
house in a face-house discrimination task). Note that this does not
mean that we assume that decision makers adjust their bias after
having seen the stimulus. Rather, we want to measure response-bias (in
favor of face or house) while assuming the same drift rate for both
stimuli. We can achieve this for accuracy coded data by modeling the
bias as moved towards the correct response boundary for one stimulus
(e.g. ``z = .6`` for houses) and away from the correct response boundary
for the other stimulus (``1-z = .4`` for faces).

First, we need to import the required python modules.
::

    import hddm
    from patsy import dmatrix  # for generation of (regression) design matrices
    import numpy as np         # for basic matrix operations
    from pandas import Series  # to manipulate data-frames generated by hddm

We save the output of stdout to the file ``ModelRecoveryOutput.txt``.
::

    import sys
    sys.stdout = open('ModelRecoveryOutput.txt', 'w')

Creating simulated data for the experiment
******************************************

Next we set the number of subjects and the number of trials per level
for the simulated experiment ::

    n_subjects = 10
    trials_per_level = 150 # and per stimulus

Next we set up parameters of the drift diffusion process for the three
levels and the first stimulus. As desribed earlier ``v`` and ``z`` change
accross levels ::

    level1a = {'v':.3, 'a':2, 't':.3, 'sv':0, 'z':.5, 'sz':0, 'st':0}
    level2a = {'v':.4, 'a':2, 't':.3, 'sv':0, 'z':.6, 'sz':0, 'st':0}
    level3a = {'v':.5, 'a':2, 't':.3, 'sv':0, 'z':.7, 'sz':0, 'st':0}

Now we generate the data for stimulus A

::

    data_a, params_a = hddm.generate.gen_rand_data({'level1': level1a,
                                                    'level2': level2a,
						    'level3': level3a},
						    size=trials_per_level,
						    subjs=n_subjects)

Next come the parameters for the second stimulus, where ``v`` is the same
as for the first stimulus. This is different for ``z``. In particular:
``z(stimulus_b) = 1 - z(stimulus_a)``. As a result, responses are
altogether biased towards responding A. Because we use accuracy coded
data, stimulus A is biased towards correct responses, and stimulus B
towards incorrect responses.  ::

    level1b = {'v':.3, 'a':2, 't':.3,'sv': 0, 'z':.5, 'sz': 0, 'st': 0}
    level2b = {'v':.4, 'a':2, 't':.3,'sv': 0, 'z':.4, 'sz': 0, 'st': 0}
    level3b = {'v':.5, 'a':2, 't':.3,'sv': 0, 'z':.3, 'sz': 0, 'st': 0}

Now we generate the data for stimulus B

::

    data_b, params_b = hddm.generate.gen_rand_data({'level1': level1b,
                                                    'level2': level2b,
                                                    'level3': level3b},
						    size=trials_per_level,
						    subjs=n_subjects)

We add a column to the ``DataFrame`` identifying stimulus A as 1 and stimulus B as 2.

::

    data_a['stimulus'] = Series(np.ones((len(data_a))), index=data_a.index)
    data_b['stimulus'] = Series(np.ones((len(data_b)))*2, index=data_a.index)

Now we merge the data for stimulus A and B

::

    mydata = data_a.append(data_b, ignore_index=True)

Setting up the HDDM regression model
************************************

Next we need to ensure that the bias is ``z`` for one stimulus and ``1-z``
for the other stimulus.  This is implemented here for all stimulus A trials
and -1 for stimulus B trials. We use the ``patsy`` command ``dmatrix`` to
generate such an array from the stimulus column of our simulated data
::

    def z_link_func(x, data=mydata):
        stim = (np.asarray(dmatrix('0 + C(s, [[0], [1]])',
                                  {'s': data.stimulus.loc[x.index]}))
        )
        # Apply z = (1 - x) to flip them along 0.5
        z_flip = stim - x
        # The above inverts those values we do not want to flip,
        # so invert them back
        z_flip[stim == 0] *= -1
        return z_flip

(NOTE: earlier versions of this tutorial suggested applying an inverse logit
link function to the regression, but this should no longer be used given changes to the prior 
on the intercept.) 

Also depending on your python version, the above code may give you errors and you can try this instead:
::

    def z_link_func(x, data=mydata):
        stim = (np.asarray(dmatrix('0 + C(s, [[0], [1]])',
                                  {'s': data.stimulus.loc[x.index]},return_type='dataframe'))
        )
        # Apply z = (1 - x) to flip them along 0.5
        z_flip = np.subtract(stim, x.to_frame())
        # The above inverts those values we do not want to flip,
        # so invert them back
        z_flip[stim == 0] *= -1
        return z_flip


Now we set up the regression models for ``z`` and ``v`` and also include the
link functions The relevant string here used by ``patsy`` is '1 +
C(condition)'. This will generate a design matrix with an intercept
(that's what the '1' is for) and two dummy variables for remaining
levels. (The column in which the levels are coded has the default name
'condition'):
::

    z_reg = {'model': 'z ~ 1 + C(condition)', 'link_func': z_link_func}

For ``v`` the link function is simply ``x = x``, because no transformations is
needed. [However, you could also analyze this experiment with response
coded data. Then you would not stimulus code ``z`` but ``v`` and you would
have to multiply the ``v`` for one condition with ``-1``, with a link function
like the one for ``z`` above, but with out the additional logit transform
]:
::

    v_reg = {'model': 'v ~ 1 + C(condition)', 'link_func': lambda x: x}

Now we can finally put the regression description for the hddm model
together. The general for this is ``[{'model': 'outcome_parameter ~ patsy_design_string', 'link_func': your_link_function }, {...}, ...]``
::

    reg_descr = [z_reg, v_reg]

The last step before running the model is to construct the complete hddm regression model by adding data etc.
::

    m_reg = hddm.HDDMRegressor(mydata, reg_descr, include='z')

Now we start the model, and wait for a while (you can go and get
several coffees, or read a paper). 
::

    m_reg.sample(5000, burn=200)

Comparing generative and recovered model parameters
***************************************************

First we print the model stats
::

    m_reg.print_stats()

Here is the relevant output for our purposes (in this case I fit a single subject, ie. I set n_subjects =1 above)

.. parsed-literal::

                               mean        std       2.5q       25q        50q       75q     97.5q       mc err
			        
				
     a                        2.01142  0.0326427    1.94747   1.98924    2.00941   2.03399   2.07567   0.00238618

     t                        0.296854  0.0077349   0.279701  0.291899   0.297717  0.302649  0.310614  0.000605895
	
     z_Intercept              0.480266  0.0167311   0.449494  0.469451   0.480738   0.49086  0.514289   0.00148616
 
     z_C(condition)[T.level2] 0.120887  0.0233508  0.0740366  0.105916   0.122639  0.136614  0.168063   0.00180898

     z_C(condition)[T.level3] 0.213324  0.0215305   0.165814  0.200958   0.213415  0.228675  0.250721   0.00190894

     v_Intercept              0.283547  0.0542307   0.172041  0.246596   0.281602  0.321173  0.400883   0.00437291

     v_C(condition)[T.level2] 0.0774754  0.0811844 -0.0850003  0.024219  0.0756668  0.130212  0.244286   0.00642687

     v_C(condition)[T.level3] 0.22311  0.0846739  0.0460032  0.160987     0.2271  0.290661  0.381936   0.00639282
 
   
Lets first look at ``v``. For ``level1`` this is just the
intercept. The value of ``.283`` is in the ball park of the true value
of ``.3``. The fit is not perfect, but running a longer chain might
help (we are ignoring sophisticated checks of model convergence for
this example here). To get the values of ``v`` for levels 2 and 3, we
have to add the respective parameters (``0.077`` and ``.22``) to the
intercept value. The resulting values of  are again
close enough to the true values of ``.4`` and ``.5``. The ``z_Intercept``
value of 0.48 is close tothe true value of ``.5``, and the level 2 and level 3
offsets are also close (.48 + .12= 0.6 and .48+.21 = 0.69).   In sum,
``HDDMRegression`` easily recovered the right order of the parameters
``z``. The recovered parameter values are also close to the true
parameter values, and this was only for a single subject fit
- parameter estimates are improved with more subjects.  
