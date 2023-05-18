#The next thing is some large experiments with lots of subjects.  

#Fit full model to each subject and look at variability in model parameters
#do bootstrap samples and fits group data from 4 or 10 observations per subject grouped 



#here is another idea where we generate data using a single set of parameters for a subject
# then we just do that for different numbers of trials. In principle you only need the big simulated data
#then you sample from that, but I am just generating another set of less trials to hold the data 
# in a manner that conforms to what I have already done
source(here::here("scripts", "R", "create-sa-sim-params.R"))

apa <- function(x, title = " ") {
  gt(x) %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left")
}


if (!file.exists(here::here("scripts", "R", "subject_generated_sim_params.RData"))) {
  
  nts <- c(4, 10, 50, 125, 250)
  ns <- 1 # number of subjects
  
  res_parameter_sets <- vector(mode = "list", length = length(nts))
  
  parameter_set <- vector(mode = "list", length = 1)
  names(parameter_set) <- letters[1]
  # speed, acc, ter, eta, sa, st, v1, v2, v3, v4
  parameter_set <- c(0.08, 0.16, 0.3, 0.08, 0.15, 0.1, 0.4, 0.25, 0.1, 0.0) #set e
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

#bootstrap samples of the datasets

# Create a sample dataframe
nboots = 300
# nts <- c(4, 10, 50, 125, 250) #number of trials
nts <- c(1, 4, 10, 50, 125, 250) #number of trials

ns <- 1 # number of subjects

res_boot_datasets <- vector(mode = "list",length = length(nts))
for (i in 1:length(nts)) {
  res_boot_datasets[[i]] <- rep(list(NULL),nboots)
}

df_temp <- res_parameter_sets$NumberObservations_250$dataset
# df_temp <- res_parameter_sets$NumberObservations_125$dataset

#get only a single instructions
df_temp <- df_temp[df_temp$instructions == "speed",]

for (foo in 1:length(nts)){

  df_tempv1 <- df_temp[df_temp$difficulty == 1,]
  df_tempv2 <- df_temp[df_temp$difficulty == 2,]
  df_tempv3 <- df_temp[df_temp$difficulty == 3,]
  df_tempv4 <- df_temp[df_temp$difficulty == 4,]
  
for (fee in 1:nboots){
  
  boot_tempv1 <- df_tempv1[sample(1:nrow(df_tempv1), nts[foo], replace = TRUE),]
  boot_tempv2 <- df_tempv2[sample(1:nrow(df_tempv2), nts[foo], replace = TRUE),]
  boot_tempv3 <- df_tempv3[sample(1:nrow(df_tempv3), nts[foo], replace = TRUE),]
  boot_tempv4 <- df_tempv4[sample(1:nrow(df_tempv4), nts[foo], replace = TRUE),]
  
  res_boot_datasets[[foo]][[fee]] <- rbind(boot_tempv1,boot_tempv2,boot_tempv3, boot_tempv4)
  
}
}
names(res_boot_datasets) <- do.call(rbind,map(nts, function(.){paste0("NumberObservations_",.)} ))

# PREPARE FILES FOR FORTRAN
# set_idx = 1

for (set_idx in 1:nboots) {
  data_set <-  res_boot_datasets[[1]][[set_idx]]  #where 5 is 250 trials
  data_set$nboots <- set_idx
  prepare_fortran(data_set, letters[[5]], method = 1, condition = "SPEED", boots = TRUE)
}


############
library(dplyr)
library(tidyr)
library(gt)

labels <- c("a_speed", "t", "eta", "sa", "z/a","st","po", "v_1", "v_2","v_3", "v_4", "likelihood")

est_data_e_250 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_250.txt",col.names = labels)
est_data_e_250 <- est_data_e_250[seq_len(nrow(est_data_e_250)) %% 2 == 0,]
est_data_e_250$parameter_set <- "e"
est_data_e_250$num_trials <- 250

est_data_e_125 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_125.txt",col.names = labels)
est_data_e_125 <- est_data_e_125[seq_len(nrow(est_data_e_125)) %% 2 == 0,]
est_data_e_125$parameter_set <- "e"
est_data_e_125$num_trials <- 125

est_data_e_50 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_50.txt",col.names = labels)
est_data_e_50 <- est_data_e_50[seq_len(nrow(est_data_e_50)) %% 2 == 0,]
est_data_e_50$parameter_set <- "e"
est_data_e_50$num_trials <- 50

est_data_e_10 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_10.txt",col.names = labels)
est_data_e_10 <- est_data_e_10[seq_len(nrow(est_data_e_10)) %% 2 == 0,]
est_data_e_10$parameter_set <- "e"
est_data_e_10$num_trials <- 10

est_data_e_4 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_4.txt",col.names = labels)
est_data_e_4 <- est_data_e_4[seq_len(nrow(est_data_e_4)) %% 2 == 0,]
est_data_e_4$parameter_set <- "e"
est_data_e_4$num_trials <- 4

est_data_e_1 <- read.table("~/Documents/hddm/scripts/fortran/res_boot_e_1.txt",col.names = labels)
est_data_e_1 <- est_data_e_1[seq_len(nrow(est_data_e_1)) %% 2 == 0,]
est_data_e_1$parameter_set <- "e"
est_data_e_1$num_trials <- 1

# est_data_all <- rbind(est_data_e_250,est_data_e_125, 
#                       est_data_e_50,est_data_e_10, est_data_e_4
# )
est_data_all <- rbind(est_data_e_250,est_data_e_125, 
                      est_data_e_50,est_data_e_10,
                      est_data_e_4, est_data_e_1)

se <- function(x){sqrt(sum((x-mean(x))^2/(length(x)-1)))/sqrt(length(x))}

table_dat <- est_data_all %>% 
  group_by(parameter_set) %>% 
  dplyr::select(-c(po,likelihood)) %>% 
  pivot_longer(-c(parameter_set, num_trials),
               names_to = "parameters",
               values_to = "gen_estimates") %>% 
  group_by(parameter_set, num_trials, parameters) %>% 
  summarise(mean = round(mean(gen_estimates),4),
            sd = round(sd(gen_estimates),4),
            se = round(se(gen_estimates), 4)
            ) %>% 
  pivot_longer(-c("parameter_set","num_trials","parameters"),
               names_to = "stats",
               values_to = "estimates") %>% 
  pivot_wider(names_from = parameters,
              values_from = estimates,) 


true_val <- res_parameter_sets$NumberObservations_250$parameters[,c("a_speed","eta","sa","st", "t", 
                                                        "v_1", "v_2","v_3", "v_4")]
true_val
table_dat[order(table_dat$stats),]  %>%  group_by(stats) %>% apa(
  "Means and Standard Deviations of Parameter Values Recovered
From the SIMPLEX Fitting Method Across Varying Numbers of Observations") %>% 
  tab_footnote(
    rbind(names(true_val))) %>% 
  tab_footnote(
    rbind(true_val))
  
# ) %>% 
#   cols_label(
#     stats = "Parameter set",
#     a_accuracy = "a",
#     t = "Ter",
#   )



