# create-sa-sim-params.R - generates parameter combinations with variation in
# Ratcliff diffusion model
#
# Copyright (C) 2023 Blair Shevlin & Kianté Fernandez, <kiantefernan@gmail.com>
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
# 2023/01/05    Blair Shevlin                         wrote original code
# 2023/01/07    Kianté Fernandez                      refactored for function output

# Libraries
library(purrr) # Functional Programming Tools
library(here)
library(rtdists)

# create function to generate subject samples
generate_subject_parameters <- function(genparam, ns) {
  # generate_subject_parameters:  Generates parameter combinations
  #
  # Arguments
  # ----------
  # genparam: a list contains all of the group level parameters
  # 
  # 
  # ns: the number of subjects to simulate
  # 
  # Returns
  # -------
  # A dataframe of the set of parameters for each subjects
  
  if (sign(genparam$t_mu - genparam$t_sd) == -1) stop("ter is less than one. select new t_sd  non-negative to avoid")

  params_temp <- data.frame(subj_idx = 1:ns)

  # Subj-level parameters
  params_temp$a_speed <- runif(ns, min = genparam$a_speed_mu - genparam$sa, max = genparam$a_speed_mu + genparam$sa)
  params_temp$a_accuracy <- runif(ns, min = genparam$a_accuracy_mu - genparam$sa, max = genparam$a_accuracy_mu + genparam$sa)
  params_temp$v_1 <- rnorm(ns, genparam$v_1_mu, genparam$v_sd)
  params_temp$v_2 <- rnorm(ns, genparam$v_2_mu, genparam$v_sd)
  params_temp$v_3 <- rnorm(ns, genparam$v_3_mu, genparam$v_sd)
  params_temp$v_4 <- rnorm(ns, genparam$v_4_mu, genparam$v_sd)
  params_temp$t <- runif(ns, min = genparam$t_mu - genparam$t_sd, max = genparam$t_mu + genparam$t_sd)

  return(params_temp)
}

generate_sa_simulations <- function(genparam,sa,nt,ns){
  # generate_sa_simulations:  Generates data according to provided group level parameters and different sa value
  #
  # Arguments
  # ----------
  # genparam: a list contains all of the group level parameters
  # 
  # sa: vector of each of the candidate sa values 
  # 
  # nt: the number of trials per condition
  # 
  # ns: the number of subjects to simulate
  # 
  # Returns
  # -------
  # List with contains each of the simulations results for each level of sa.
  # Each list contains two dataframes: data set and parameters.
  # dataset is the organized data set for all the conditions
  # parameters is the corresponding set of parameters used to generate subject data
  
  sim_res <- vector(mode = "list", length = length(sa))
  
  for (sa_idx in seq_along(sa)) {
    # change sa parameter value
    genparam$sa <- sa[[sa_idx]]
    # generate params
    parameters <- generate_subject_parameters(genparam, ns) # function works
    
    # subj_idx <- 1
    # create list for each of the conditions (instructions and diff)
    temp_ds1 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds2 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds3 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds4 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds5 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds6 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds7 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds8 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    
    for (subj_idx in seq_along(parameters$subj_idx)) {
      # apply parameters to each condition
      # speed
      temp_ds1[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_speed[[subj_idx]], v = parameters$v_1[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds2[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_speed[[subj_idx]], v = parameters$v_2[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds3[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_speed[[subj_idx]], v = parameters$v_3[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds4[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_speed[[subj_idx]], v = parameters$v_4[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      # acc
      temp_ds5[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_accuracy[[subj_idx]], v = parameters$v_1[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds6[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_accuracy[[subj_idx]], v = parameters$v_2[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds7[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_accuracy[[subj_idx]], v = parameters$v_3[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
      temp_ds8[subj_idx] <- list(rtdists::rdiffusion(n = nt, a = parameters$a_accuracy[[subj_idx]], v = parameters$v_4[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * parameters$a_speed[[subj_idx]]))
    }
    ds <- list(temp_ds1, temp_ds2, temp_ds3, temp_ds4, temp_ds5, temp_ds6, temp_ds7, temp_ds8)
    # clean up the datasets
    org_res <- vector(mode = "list", length = length(ds))
    for (ds_idx in 1:8) {
      temp_dat <- data.frame(subject_idx = unlist(purrr::map(seq(1, ns), rep, times = nt)), do.call(rbind, ds[[ds_idx]]))
      
      if (ds_idx < 5) {
        temp_dat$instructions <- "speed"
      } else {
        temp_dat$instructions <- "accuracy"
      }
      
      if (ds_idx %in% c(1, 5)) {
        temp_dat$difficulty <- 1
      } else if (ds_idx %in% c(2, 6)) {
        temp_dat$difficulty <- 2
      } else if (ds_idx %in% c(3, 7)) {
        temp_dat$difficulty <- 3
      } else {
        temp_dat$difficulty <- 4
      }
      temp_dat$sa <- sa[[sa_idx]]
      org_res[[ds_idx]] <- temp_dat
    }
    
    sim_res[[sa_idx]] <- list(dataset = do.call(rbind, org_res), parameters = parameters)
  }
  return(sim_res)
}

