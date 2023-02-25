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
# Group-level variability
genparam$t_sd <- 0.2
genparam$v_sd <- 0.1040 # this is our `eta` for our purposes
genparam$a_sd <- 0.02

# select a range of sa values
sa <- c(0, .001, .01, .05, .07)

nt <- 300 #this generates 300 trials per condition and difficulty

ns <- 50 # number of subjects

#save the res for later reference (but we dont need to save everytime)
if (!file.exists(here::here("scripts", "R", "generated_sim_params.RData"))) {
  res <- generate_sa_simulations(genparam, sa, nt, ns)
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

# labels <- c("a", "ter", "eta", "sa", "z/a","st","po", "v1", "v2","v3", "v4", "likelihood")

labels <- c("a_acc","a_speed", "ter", "eta", "sa", "z/a","st","po", "v1", "v2","v3", "v4", "likelihood")

res_1 <- read.table("~/Documents/OSU/hddm/scripts/fortran/fort.101",col.names = labels)
res_2 <- read.table("~/Documents/OSU/hddm/scripts/fortran/fort.102",col.names = labels)
res_3 <- read.table("~/Documents/OSU/hddm/scripts/fortran/fort.103",col.names = labels)
res_4 <- read.table("~/Documents/OSU/hddm/scripts/fortran/fort.104",col.names = labels)
res_5 <- read.table("~/Documents/OSU/hddm/scripts/fortran/fort.105",col.names = labels)

# TODO fitting plot subject parameter gen_value est_value then do the diagonal 
# combine the speed and acc within same plot so you can see if one condition over
#another if fitting well

