
library(here)
source(here::here("scripts","R", "create-sa-sim-params.R"))
# set seed
set.seed(555)

### Simulations ###
#create a list with the population level parameters
genparam <- vector(mode = "list")

# Group-level means
genparam$a_speed_mu <- 0.91
genparam$a_accuracy_mu <- 1.48
genparam$v_1_mu <- 3.30
genparam$v_2_mu <- 2.05
genparam$v_3_mu <- 1.08
genparam$v_4_mu <- 0.43
genparam$t_mu <- 0.28
# Group-level variability
genparam$t_sd <- 0.175
genparam$v_sd <- 0.25 # this is our `eta` for our purposes

#select a range of sa values
sa <- c(0, .01, .1, .5, .75)
nt <- 1000 # number of trials
ns <- 100 # number of subjects

res <- generate_sa_simulations(genparam, sa, nt, ns)

#pretty simple to now use a different number of trials where we scale back the number of samples
