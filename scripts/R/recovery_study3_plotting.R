#The next thing is some large experiments with lots of subjects.  

#Fit full model to each subject and look at variability in model parameters
#do bootstrap samples and fits group data from 4 or 10 observations per subject grouped 



#here is another idea where we generate data using a single set of parameters for a subject
# then we just do that for different numbers of trials. In principle you only need the big simulated data
#then you sample from that, but I am just generating another set of less trials to hold the data 
# in a manner that conforms to what I have already done
if (!file.exists(here::here("scripts", "R", "subject_generated_sim_params.RData"))) {
  
  nts <- c(4, 10, 50, 125, 250)
  ns <- 1 # number of subjects
  
  res_parameter_sets <- vector(mode = "list", length = length(nts))
  
  parameter_set <- vector(mode = "list", length = 1)
  names(parameter_set) <- letters[1]
  # speed, acc, ter, eta, sa, st, v1, v2, v3, v4
  parameter_set <- c(0.08, 0.16, 0.3, 0.08, 0.15, 0.1, 0.4, 0.25, 0.1, 0.0)
  parmaset_temp <- parameter_set
  genparam <- vector(mode = "list")
  # Group-level means
  genparam$a_speed_mu <- parmaset_temp[[1]]
  genparam$a_accuracy_mu <- parmaset_temp[[2]]
  genparam$t_mu <- parmaset_temp[[3]]
  # Group-level variability
  genparam$v_sd <- parmaset_temp[[4]]
  genparam$sa <- parmaset_temp[[5]]
  genparam$t_sd <- parmaset_temp[[6]]
  # drift rates
  genparam$v_1_mu <- parmaset_temp[[7]]
  genparam$v_2_mu <- parmaset_temp[[8]]
  genparam$v_3_mu <- parmaset_temp[[9]]
  genparam$v_4_mu <- parmaset_temp[[10]]
  
  for (num_trials in 1:length(nts)){
    nt_temp <- nts[[num_trials]] # this generates trials per condition and difficulty
    res_parameter_sets[num_trials] <- generate_sa_simulations(genparam, genparam$sa, nt_temp, ns)
  }
  names(res_parameter_sets) <- do.call(rbind,map(nts, function(.){paste0("NumberObservations_",.)} ))
  
  save(parameter_set, res_parameter_sets, file = here::here("scripts", "R", "subject_generated_sim_params.RData"))
} else {
  load(here::here("scripts", "R", "subject_generated_sim_params.RData"))
}

