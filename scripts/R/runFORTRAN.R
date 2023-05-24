source(here::here("scripts", "R", "create-sa-sim-params.R"))

# Load the necessary library
library(tools)
# Set the path and filename of the original file
original_path <- "/Users/fernandez.332/Documents/hddm/scripts/fortran/fort.12"
# Define the source folder
source_folder <- "/Users/fernandez.332/Documents/hddm/scripts/fortran/"
# Define the path to the shell script file
script_file <- "start_intel.sh"

# Compile the FORTRAN program
getwd()
setwd(here::here("scripts", "fortran"))

# Create a sample dataframe
nboots <- 300
nts <- c(1, 4, 10, 50, 125, 250) # number of trials
ns <- 1 # number of "subjects" because group model
letter_index <- 5 #e
# letter_index <- 13 #m

if (!file.exists(here::here("scripts", "R", "subject_generated_sim_params.RData"))) {
  
  nts <- c(1, 4, 10, 50, 125, 250)
  ns <- 1 # number of subjects
  
  res_parameter_sets <- vector(mode = "list", length = length(nts))
  
  parameter_set <- vector(mode = "list", length = 1)
  names(parameter_set) <- letters[letter_index]
  # speed, acc, ter, eta, sa, st, v1, v2, v3, v4
  parameter_set <- c(0.08, 0.16, 0.3, 0.08, 0.15, 0.1, 0.4, 0.25, 0.1, 0.0) #set e
  # parameter_set <- c(0.08, 0.16, 0.3, 0.20, 0.15, 0.1, 0.4, 0.25, 0.1, 0.0) #set m
  
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
# nts_index = 3
### his all below goes in the loop
for (nts_index in 3:length(nts)) {
  res_boot_datasets <- vector(mode = "list", length = length(nts))

  for (i in 1:length(nts)) {
    res_boot_datasets[[i]] <- rep(list(NULL), nboots)
  }

  df_temp <- res_parameter_sets$NumberObservations_250$dataset

  # get only a single instructions
  # df_temp <- df_temp[df_temp$instructions == "speed", ]
  df_temp <- df_temp[df_temp$instructions == "accuracy",]

  for (foo in 1:length(nts)) {
    df_tempv1 <- df_temp[df_temp$difficulty == 1, ] #.4
    # df_tempv2 <- df_temp[df_temp$difficulty == 2, ]
    df_tempv3 <- df_temp[df_temp$difficulty == 3, ] #.1
    # df_tempv4 <- df_temp[df_temp$difficulty == 4, ]

    for (fee in 1:nboots) {
      boot_tempv1 <- df_tempv1[sample(1:nrow(df_tempv1), nts[foo], replace = TRUE), ]
      # boot_tempv2 <- df_tempv2[sample(1:nrow(df_tempv2), nts[foo], replace = TRUE), ]
      boot_tempv3 <- df_tempv3[sample(1:nrow(df_tempv3), nts[foo], replace = TRUE), ]
      # boot_tempv4 <- df_tempv4[sample(1:nrow(df_tempv4), nts[foo], replace = TRUE), ]

      # res_boot_datasets[[foo]][[fee]] <- rbind(boot_tempv1, boot_tempv2, boot_tempv3, boot_tempv4)
      res_boot_datasets[[foo]][[fee]] <- rbind(boot_tempv1, boot_tempv3)
      
    }
  }
  names(res_boot_datasets) <- do.call(rbind, purrr::map(nts, function(.) {
    paste0("NumberObservations_", .)
  }))
  # nts_index = 1
  # PREPARE FILES FOR FORTRAN
  for (set_idx in 1:nboots) {
    data_set <- res_boot_datasets[[nts_index]][[set_idx]] # where 6 is 250 trials
    data_set$nboots <- set_idx
    # prepare_fortran(data_set, letters[[letter_index]], method = 1, condition = "SPEED", boots = TRUE)
    
    prepare_fortran(data_set, letters[[letter_index]], method = 1, condition = "ACCURACY", boots = TRUE)
  }
  
  # Define the script content
  script_content <- '. /opt/intel/oneapi/setvars.sh\n'
  script_content <- paste(script_content, 'ulimit -s unlimited\n', sep = '')
  
  script_content <- paste(script_content, 'ifort -O3 {filename}.f -qopenmp -qmkl\n', sep = '')
  
  filename <- as.character(stringr::str_glue("fit_sa_SIMPLEX_",nts[nts_index])) # Replace with the desired filename
  
# "/Users/fernandez.332/Documents/hddm/scripts/fortran/fortran_2_drifts/fit_sa_SIMPLEX_1.f"

  argument <- as.character(stringr::str_glue(".", letters[[letter_index]], ".fast-dm.csv"))
  quoted_argument <- shQuote(argument)
  
  script_content <- stringr::str_glue(script_content, './a.out {quoted_argument}\n')
  
  # Replace the filename placeholder with the actual filename
  script_content <- stringr::str_replace(script_content, "\\{filename\\}", filename)
  
  # Write the script content to a file
  cat(script_content, file = script_file)
  
  # Make the script file executable
  Sys.chmod(script_file, "0777")
  
  # Run the shell script using system2
  system2("/bin/sh", script_file)
  
  # Set the new path and filename for the renamed file
  new_path <- stringr::str_glue("/Users/fernandez.332/Documents/hddm/scripts/fortran/res_boot_", letters[[letter_index]], "_", nts[nts_index], ".txt")
  # Rename the results file
  file.rename(original_path, new_path)

  # Define the destination folder
  destination_folder <- stringr::str_glue("/Users/fernandez.332/Documents/hddm/scripts/fortran/datasets/", letters[[letter_index]], "_", nts[nts_index], "_boot")
  # Create the destination folder if it doesn't exist
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }

  # List all CSV files in the source folder
  csv_files <- list.files(source_folder, pattern = "*.csv", full.names = TRUE)

  # Move each CSV file to the destination folder
  for (file in csv_files) {
    file.rename(file, file.path(destination_folder, basename(file)))
  }
}
