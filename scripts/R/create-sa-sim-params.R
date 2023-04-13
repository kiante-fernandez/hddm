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
# 2023/02/12    Kianté Fernandez                      updated to capture trial variation per subject

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
  # for the case  where we add variation to a for each subject
  # params_temp$a_speed <- runif(ns, min = genparam$a_speed_mu - genparam$a_sd, max = genparam$a_speed_mu + genparam$a_sd)
  # params_temp$a_accuracy <- runif(ns, min = genparam$a_accuracy_mu - genparam$a_sd, max = genparam$a_accuracy_mu + genparam$a_sd)

  params_temp$a_speed <- genparam$a_speed_mu
  params_temp$a_accuracy <- genparam$a_accuracy_mu

  # note the sd change here to 0
  params_temp$v_1 <- rnorm(ns, genparam$v_1_mu, 0)
  params_temp$v_2 <- rnorm(ns, genparam$v_2_mu, 0)
  params_temp$v_3 <- rnorm(ns, genparam$v_3_mu, 0)
  params_temp$v_4 <- rnorm(ns, genparam$v_4_mu, 0)

  # params_temp$t <- runif(ns, min = genparam$t_mu - genparam$t_sd, max = genparam$t_mu + genparam$t_sd)
  params_temp$t <- genparam$t_mu

  params_temp$st <- genparam$t_sd
  params_temp$eta <- genparam$v_sd

  params_temp$sa <- genparam$sa
  
  return(params_temp)
}

generate_sa_simulations <- function(genparam, sa, nt, ns) {
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
  # sa_idx = 5
  parameters <- generate_subject_parameters(genparam, ns)
  # sa_idx = 5
  for (sa_idx in seq_along(sa)) {
    # change sa parameter value
    genparam$sa <- sa[[sa_idx]]
    # generate params

    # create list for each of the conditions (instructions and diff)
    temp_ds1 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds2 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds3 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds4 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds5 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds6 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds7 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    temp_ds8 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
    # subj_idx = 1
    for (subj_idx in seq_along(parameters$subj_idx)) {
      # old def
      # a_speed_temp <- runif(nt, min = parameters$a_speed[[subj_idx]] - genparam$sa, max = parameters$a_speed[[subj_idx]] + genparam$sa)
      # a_accuracy_temp <- runif(nt, min = parameters$a_accuracy[[subj_idx]] - genparam$sa, max = parameters$a_accuracy[[subj_idx]] + genparam$sa)
      # RR def
      a_speed_temp <- runif(nt, min = parameters$a_speed[[subj_idx]] - genparam$sa / 2, max = parameters$a_speed[[subj_idx]] + genparam$sa / 2)
      a_accuracy_temp <- runif(nt, min = parameters$a_accuracy[[subj_idx]] - genparam$sa / 2, max = parameters$a_accuracy[[subj_idx]] + genparam$sa / 2)
      # speed
      temp_ds1[subj_idx] <- list(map_df(a_speed_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_1[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds2[subj_idx] <- list(map_df(a_speed_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_2[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds3[subj_idx] <- list(map_df(a_speed_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_3[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds4[subj_idx] <- list(map_df(a_speed_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_4[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      # accuracy
      temp_ds5[subj_idx] <- list(map_df(a_accuracy_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_1[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds6[subj_idx] <- list(map_df(a_accuracy_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_2[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds7[subj_idx] <- list(map_df(a_accuracy_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_3[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
      temp_ds8[subj_idx] <- list(map_df(a_accuracy_temp, function(.) {
        rtdists::rdiffusion(n = 1, a = ., v = parameters$v_4[[subj_idx]], sv = parameters$eta[[subj_idx]], st0 = parameters$st[[subj_idx]], t0 = parameters$t[[subj_idx]], z = 0.5 * ., s = 0.1)
      }))
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


prepare_fortran <- function(res, experiment, method = 1, condition = "ACCURACY", boots = FALSE) {
  # prepare_fortran:  organizes and saves the data for the FORTRAN fitting procedure
  #
  # Arguments
  # ----------
  # res: the output of ``generate_sa_simulations`. it is a list`
  # res: the output of ``generate_sa_simulations`. it is a df from the exp 2
  #
  # condition: either "ACCURACY" or "SPEED"
  #
  # boots: are we saving bootstrap datasets T/F
  #
  # sa_condition: which level of sa to reorganize the data
  # experiment: the letter association with the parameter values set
  #
  # method: an indicator of the format of the output for fitting
  # Returns
  # -------
  # a set of csv's in the fortran folder
  #
  # temp_df <- res[[sa_condition]][["dataset"]]
  if (method == 1){
    temp_df <- res
    # change response to zero/one
    temp_df$response <- as.integer(factor(temp_df$response)) - 1
    
    # round off the RT (looks like that is what it is in the sheet too)
    temp_df$rt <- round(temp_df$rt, 4)
    # select a subset of the data from the condition of interest
    temp_df$difficulty <- factor(temp_df$difficulty, levels = c(1, 2, 3, 4), labels = c("high", "low", "vlow", "nonword"))
    if (condition == "SPEED") {
      temp_df <- temp_df[temp_df$instructions == "speed", ]
    } else if (condition == "ACCURACY") {
      temp_df <- temp_df[temp_df$instructions == "accuracy", ]
    }
    # create one string for the condition
    temp_df <- tidyr::unite(temp_df, col = "condition", c("difficulty", "instructions"), sep = " ")
    temp_df <- tidyr::unite(temp_df, col = "data", c("response", "rt", "condition"), sep = " ")
    
    if (boots == TRUE){
        boots_idx <- unique(res$nboots) 
        if (boots_idx < 10) {
          temp_file_name <- paste0("boot00", boots_idx, ".", experiment, ".fast-dm.csv")
        } else if (boots_idx < 100){
          temp_file_name <- paste0("boot0", boots_idx, ".", experiment, ".fast-dm.csv")
        } else {
          temp_file_name <- paste0("boot", boots_idx, ".", experiment, ".fast-dm.csv")
        }
        write.table(temp_df, here::here("scripts", "fortran", temp_file_name), row.names = F, quote = FALSE, col.names = F)
    } else if (boots == FALSE){
      for (subject_idx in 1:length(unique(temp_df$subject_idx))) {
        
        temp_df_subject <- temp_df[temp_df$subject_id == subject_idx, "data"]
        
        if (subject_idx < 10) {
          temp_file_name <- paste0("subj00", subject_idx, ".", experiment, ".fast-dm.csv")
        } else {
          temp_file_name <- paste0("subj0", subject_idx, ".", experiment, ".fast-dm.csv")
        }
        # TODO what happens with the 1ooth subject and the fitting code
        # stringr::str_detect(temp_df_subject,"\")
        # print(temp_file_name)
        write.table(temp_df_subject, here::here("scripts", "fortran", temp_file_name), row.names = F, quote = FALSE, col.names = F)
      }
    }
    
  } else if (method == 2){
    temp_df <- res
    # temp_df <- res_parameter_sets[[letters[1]]][["dataset"]]
    #select single instruction
    temp_df <- temp_df[temp_df$instructions =="speed",]
    
    # change response to zero/one
    temp_df$response <- as.integer(factor(temp_df$response)) - 1
    temp_df$difficulty <- factor(temp_df$difficulty, levels = c(1, 2, 3, 4), labels = c("high", "low", "vlow", "nonword"))
    
    # round off the RT (looks like that is what it is in the sheet too)
    temp_df$rt <- round(temp_df$rt, 3) * 1000

    for (subject_idx in 1:length(unique(temp_df$subject_idx))) {
      temp_df_subject <- temp_df[temp_df$subject_id == subject_idx,]
      
      #define the up and down datasets
      temp_df_up <- temp_df_subject[temp_df_subject$response == 1,]
      temp_df_down <- temp_df_subject[temp_df_subject$response == 0,]
      
      c1 <- matrix(,nrow = 1 + dim(temp_df_up[temp_df_up$difficulty == "high",])[1], 1)
      c1[1] <- dim(temp_df_up[temp_df_up$difficulty == "high",])[1]
      c1[-1] <- temp_df_up[temp_df_up$difficulty == "high",]$rt[order(temp_df_up[temp_df_up$difficulty == "high",]$rt)]
      
      c2 <- matrix(,nrow = 1 + dim(temp_df_up[temp_df_up$difficulty == "low",])[1], 1)
      c2[1] <- dim(temp_df_up[temp_df_up$difficulty == "low",])[1]
      c2[-1] <- temp_df_up[temp_df_up$difficulty == "low",]$rt[order(temp_df_up[temp_df_up$difficulty == "low",]$rt)]
      
      c3 <- matrix(,nrow = 1 + dim(temp_df_up[temp_df_up$difficulty == "vlow",])[1], 1)
      c3[1] <- dim(temp_df_up[temp_df_up$difficulty == "vlow",])[1]
      c3[-1] <- temp_df_up[temp_df_up$difficulty == "vlow",]$rt[order(temp_df_up[temp_df_up$difficulty == "vlow",]$rt)]
      
      c4 <- matrix(,nrow = 1 + dim(temp_df_up[temp_df_up$difficulty == "nonword",])[1], 1)
      c4[1] <- dim(temp_df_up[temp_df_up$difficulty == "nonword",])[1]
      c4[-1] <- temp_df_up[temp_df_up$difficulty == "nonword",]$rt[order(temp_df_up[temp_df_up$difficulty == "nonword",]$rt)]
      
      ups <- rbind(c1,c2,c3,c4)
      
      c1 <- matrix(,nrow = 1 + dim(temp_df_down[temp_df_down$difficulty == "high",])[1], 1)
      c1[1] <- dim(temp_df_down[temp_df_down$difficulty == "high",])[1]
      c1[-1] <- temp_df_down[temp_df_down$difficulty == "high",]$rt[order(temp_df_down[temp_df_down$difficulty == "high",]$rt)]
      
      c2 <- matrix(,nrow = 1 + dim(temp_df_down[temp_df_down$difficulty == "low",])[1], 1)
      c2[1] <- dim(temp_df_down[temp_df_down$difficulty == "low",])[1]
      c2[-1] <- temp_df_down[temp_df_down$difficulty == "low",]$rt[order(temp_df_down[temp_df_down$difficulty == "low",]$rt)]
      
      c3 <- matrix(,nrow = 1 + dim(temp_df_down[temp_df_down$difficulty == "vlow",])[1], 1)
      c3[1] <- dim(temp_df_down[temp_df_down$difficulty == "vlow",])[1]
      c3[-1] <- temp_df_down[temp_df_down$difficulty == "vlow",]$rt[order(temp_df_down[temp_df_down$difficulty == "vlow",]$rt)]
      
      c4 <- matrix(,nrow = 1 + dim(temp_df_down[temp_df_down$difficulty == "nonword",])[1], 1)
      c4[1] <- dim(temp_df_down[temp_df_down$difficulty == "nonword",])[1]
      c4[-1] <- temp_df_down[temp_df_down$difficulty == "nonword",]$rt[order(temp_df_down[temp_df_down$difficulty == "nonword",]$rt)]
      
      downs <- rbind(c1,c2,c3,c4)
      
      if (subject_idx < 10) {
        temp_file_name_up <- paste0("up.","subj00", subject_idx, ".", experiment, ".fast-dm.csv")
        temp_file_name_down <- paste0("down.","subj00", subject_idx, ".", experiment, ".fast-dm.csv")
      } else {
        temp_file_name_up <- paste0("up.","subj0", subject_idx, ".", experiment, ".fast-dm.csv")
        temp_file_name_down <- paste0("down.","subj0", subject_idx, ".", experiment, ".fast-dm.csv")
      }
      write.table(ups, here::here("scripts", "fortran", temp_file_name_up), row.names = F, quote = FALSE, col.names = F)
      write.table(downs, here::here("scripts", "fortran", temp_file_name_down), row.names = F, quote = FALSE, col.names = F)
    }
  }
}


