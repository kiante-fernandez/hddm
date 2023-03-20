########### parameter recovery study 2 plotting code##########

# Mean parameters adapted from table 2 in recovery study from:
# Ratcliff, R., & Tuerlinckx, F. (2002). 
# Estimating parameters of the diffusion model: Approaches to dealing with 
# contaminant reaction times and parameter variability. 
# Psychonomic Bulletin and Review, 9, 438-481. 


library(tidyverse)
library(patchwork)
them <- theme(
  axis.title.x = element_text(
    face = "bold", size = 12,
    margin = margin(b = 20, t = 15)
  ),
  axis.text.x = element_text(
    size = 12, color = "black", hjust = 0.4,
    margin = margin(t = 3)
  ),
  axis.title.y = element_text(
    face = "bold", angle = 90, size = 12,
    margin = margin(l = 20, r = 15)
  ),
  axis.text.y = element_text(
    size = 12, color = "black", vjust = 0.45,
    margin = margin(r = 3)
  ),
  axis.line = element_line(color = "black", size = 0),
  axis.ticks.length = unit(5, "pt"),
  strip.text.x = element_text(
    face = "bold", size = 12,
    margin = margin(t = 10, b = 10, unit = "pt")
  ),
  strip.text.y = element_text(
    face = "bold", size = 12,
    margin = margin(t = 10, b = 10, unit = "pt")
  ),
  strip.background = element_blank(),
  # legend.position = "right",
  legend.position = "none",
  legend.text = element_text(
    size = 10, face = "bold",
    margin = margin(t = 5, b = 5, r = 18, l = 0, unit = "pt")
  ),
  legend.spacing.y = unit(6, "pt"),
  legend.key.width = unit(15, "pt"),
  legend.key.height = unit(6, "pt"),
  legend.background = element_rect(colour = NA),
  legend.title = element_text(size = 11, face = "bold"),
  panel.background = element_blank(),
  panel.border = element_rect(fill = NA, color = "black", size = 1),
  panel.spacing = unit(1, "lines"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

load(here::here("scripts", "R", "exp_2_generated_sim_params.RData"))
file_name <- "~/Documents/hddm/scripts/fortran/fort.400"

compare_parameters <- function(file_name, parameter_set){
  gen_plotting_data <- res_parameter_sets[[parameter_set]][["parameters"]]
  gen_plotting_data$parameters <- "generated"
  
  labels <- c("a_accuracy","a_speed", "t", "eta", "sa", "z/a","st", "v_1", "v_2","v_3", "v_4", "likelihood")
  est_plotting_a <- read.table(file_name, col.names = labels)
  est_plotting_a$subj_idx <- 1:99
  est_plotting_a$parameters <- "estimated"
  
  data_vline <- est_plotting_a %>% 
    dplyr::select(-c(likelihood, parameters,subj_idx)) %>% 
    pivot_longer(everything(),
                 names_to = "parameters",
                 values_to = "gen_estimates") %>% 
    group_by(parameters) %>% 
    summarise(vline_1 = mean(gen_estimates)) %>% 
    filter(parameters != "z.a")
  
  data_vline$vline_2  <- as.numeric(gen_plotting_data[1,data_vline$parameters])
  
  plt_dat <- est_plotting_a %>% 
    dplyr::select(-c(likelihood, parameters,subj_idx)) %>% 
    pivot_longer(everything(),
                 names_to = "parameters",
                 values_to = "gen_estimates")
  
  a <- ggplot(plt_dat,aes(x=gen_estimates)) + geom_histogram() + facet_wrap(~parameters,scales = "free")+
    geom_vline(data =data_vline, aes(xintercept = vline_1), color = "red")+
    geom_vline(data =data_vline, aes(xintercept = vline_2),color = "blue")+
    labs(title = paste0(data_vline$parameters, collapse = " "),
         subtitle = paste0(data_vline$vline_2, collapse = " "),
         caption = "red = mean, blue = generated")+ them
  
  b <- est_plotting_a %>% 
    dplyr::select(-c(likelihood, parameters,subj_idx)) %>% 
    cor() %>% 
    ggcorrplot::ggcorrplot(type = "upper",
                           lab = TRUE)+
    theme_classic()+
    labs(x = "", y = "") +
    theme(
      axis.text = element_text(face = "bold"),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(face="bold",size=10, angle=40),
      axis.text.x = element_text(face="bold",size=10, angle=40, hjust= .9)
    ) + them
  
  return(a + b)
  
}

compare_parameters("~/Documents/hddm/scripts/fortran/fort.400", "a")
compare_parameters("~/Documents/hddm/scripts/fortran/fort.401", "b")
compare_parameters("~/Documents/hddm/scripts/fortran/fort.402", "c")

#pool and get mean and SD for each parameter
est_data_a <- read.table("~/Documents/hddm/scripts/fortran/fort.400",col.names = labels)
est_data_a$parameter_set <- "a"
est_data_b <- read.table("~/Documents/hddm/scripts/fortran/fort.401",col.names = labels)
est_data_b$parameter_set <- "b"
est_data_c <- read.table("~/Documents/hddm/scripts/fortran/fort.402",col.names = labels)
est_data_c$parameter_set <- "c"

est_data_all <- rbind(est_data_a,est_data_b,est_data_c)

table_dat <- est_data_all %>% 
  group_by(parameter_set) %>% 
  dplyr::select(-c(likelihood)) %>% 
  pivot_longer(-c(parameter_set),
               names_to = "parameters",
               values_to = "gen_estimates") %>% 
  group_by(parameter_set, parameters) %>% 
  summarise(mean = round(mean(gen_estimates),4),
            sd = round(sd(gen_estimates),4)) %>% 
  pivot_longer(-c("parameter_set","parameters"),
               names_to = "stats",
               values_to = "estimates") %>% 
  pivot_wider(names_from = parameters,
              values_from = estimates,) 
  
table_dat[order(table_dat$stats),] %>% gt::gt()
