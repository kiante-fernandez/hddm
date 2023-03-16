# sa_simulations.R - runs different simulations for the sa values
#
# Copyright (C) 2023 Kianté Fernandez, <kiantefernan@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 2023/01/07    Kianté Fernandez                      wrote original code
# 2023/02/22    Kianté Fernandez                      added saving res functionality 

library(here)
source(here::here("scripts", "R", "create-sa-sim-params.R"))

# set seed
set.seed(2023)

### Simulations ###

# Mean parameters taken from Young adults from Experiment 2 in:
# Ratcliff, R., Thapar, A., & McKoon, G. (2001).
# The effects of aging on reaction time in a signal detection task.
# Psychology and Aging, 16(2), 323–341. https://doi.org/10.1037/0882-7974.16.2.323

# create a list with the population level parameters
genparam <- vector(mode = "list")

# Group-level means
genparam$a_speed_mu <- 0.091
genparam$a_accuracy_mu <- 0.148
genparam$v_1_mu <- 0.3300
genparam$v_2_mu <- 0.2050
genparam$v_3_mu <- 0.1077
genparam$v_4_mu <- 0.0425
genparam$t_mu <- 0.2846
# genparam$t_mu <- 0.5000

# Group-level variability
genparam$t_sd <- 0.1
genparam$v_sd <- 0.04 # this is our `eta` for our purposes
genparam$a_sd <- 0.02 # is this still desired

# select a range of sa values
sa <- c(0, .001, .01, .05, .07)

nt <- 1250 #this generates trials per condition and difficulty
# nt <- 150 #this generates trials per condition and difficulty

ns <- 10 # number of subjects

#save the res for later reference (but we dont need to save everytime)
if (!file.exists(here::here("scripts", "R", "generated_sim_params.RData"))) {
  res <- generate_sa_simulations(genparam, sa, nt, ns)
  # save(res, file = here::here("scripts", "R", "generated_single_sim_params.RData"))
  
  save(res, file = here::here("scripts", "R", "generated_sim_params.RData"))
}else {
  load(here::here("scripts", "R", "generated_sim_params.RData"))
}

for (sa_idx in 1:length(sa)){
    prepare_fortran(res, sa_idx)
}

# ##Prepare data for fortran fitting
# for (condition_idx in c("SPEED","ACCURACY")){
#   for (sa_idx in 1:length(sa)){
#     prepare_fortran(res, condition_idx, sa_idx)
#   }
# }

#TODO can you pull in the results from the FORTRAN code ? Yes. see the fort. file for those you can make a table of them

#use systems command
setwd(here::here("scripts", "fortran"))
system2(command = "pwd")
system2(command = "ls")
#start intel complier
system2(command = ".", args = "/opt/intel/oneapi/setvars.sh")
#this might be how we do it:
system2(command = "ifort", args = c("-O3", "test_speed_accuracy.f","-qopenmp","-qmkl"))

#TODO allow the .f file to take and input and output command so we can run 
#     the files without have to reload them each time

###########parameter recovery study 2##########
# create a list with the population level parameters

if (!file.exists(here::here("scripts", "R", "generated_sim_params.RData"))) {
  
  res_parameter_sets <- vector(mode = "list", length = 12)
  
  parameter_sets <- vector(mode = "list", length = 12)
  names(parameter_sets) <- letters[1:12]
  # acc, speed, ter, eta, sa, st, v1, v2, v3, v4
  parameter_sets[[1]] <- c(0.08, 0.16, 0.3, 0.08, 0.00, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[2]] <-c(0.08, 0.16, 0.3, 0.08, 0.01, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[3]] <-c(0.08, 0.16, 0.3, 0.16, 0.00, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[4]] <-c(0.08, 0.16, 0.3, 0.16, 0.01, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[5]] <-c(0.08, 0.16, 0.3, 0.08, 0.05, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[6]] <-c(0.08, 0.16, 0.3, 0.16, 0.05, 0.1, 0.4, 0.25, 0.1, 0.0)
  parameter_sets[[7]] <-c(0.08, 0.16, 0.3, 0.08, 0.00, 0.1, 0.5, 0.40, 0.2, 0.1)
  parameter_sets[[8]] <-c(0.08, 0.16, 0.3, 0.08, 0.01, 0.1, 0.5, 0.40, 0.2, 0.1)
  parameter_sets[[9]] <-c(0.08, 0.16, 0.3, 0.08, 0.05, 0.1, 0.5, 0.40, 0.2, 0.1)
  parameter_sets[[10]] <-c(0.08, 0.16, 0.3, 0.16, 0.00, 0.1, 0.5, 0.40, 0.2, 0.1)
  parameter_sets[[11]] <-c(0.08, 0.16, 0.3, 0.16, 0.01, 0.1, 0.5, 0.40, 0.2, 0.1)
  parameter_sets[[12]] <-c(0.08, 0.16, 0.3, 0.16, 0.05, 0.1, 0.5, 0.40, 0.2, 0.1)
  
  nt <- 250 #this generates trials per condition and difficulty
  ns <- 100 # number of subjects
  
  for (parmaset_idx in seq_along(parameter_sets)){
    
    parmaset_temp <- parameter_sets[[parmaset_idx]]
    
    genparam <- vector(mode = "list")
    # Group-level means
    genparam$a_speed_mu <- parmaset_temp[[1]]
    genparam$a_accuracy_mu <- parmaset_temp[[2]]
    genparam$t_mu <-  parmaset_temp[[3]]
    # Group-level variability
    genparam$v_sd <- parmaset_temp[[4]]
    genparam$sa <- parmaset_temp[[5]]
    genparam$t_sd <- parmaset_temp[[6]]
    #drift rates 
    genparam$v_1_mu <-  parmaset_temp[[7]]
    genparam$v_2_mu <-  parmaset_temp[[8]]
    genparam$v_3_mu <-  parmaset_temp[[9]]
    genparam$v_4_mu <-  parmaset_temp[[10]]
    
    res_parameter_sets[[parmaset_idx]] <- generate_sa_simulations(genparam, genparam$sa, nt, ns)
    
  }
  names(res_parameter_sets) <- letters[1:12]
  
  save(parameter_sets, res_parameter_sets, file = here::here("scripts", "R", "exp_2_generated_sim_params.RData"))
}else {
  load(here::here("scripts", "R", "exp_2_generated_sim_params.RData"))
}

# set_idx = 1
#PREPARE FILES FOR FORTRAN
for (set_idx in seq_along(parameter_sets)){
  data_set <- res_parameter_sets[[letters[set_idx]]][[1]][["dataset"]]
  prepare_fortran(data_set, letters[[set_idx]])
}

