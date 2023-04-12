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

compare_parameters <- function(file_name, parameter_set){
  gen_plotting_data <- res_parameter_sets[[parameter_set]][["parameters"]]
  gen_plotting_data$parameters <- "generated"
  
  labels <- c("a_accuracy", "t", "eta", "sa", "z/a","st","po", "v_1", "v_2","v_3", "v_4", "likelihood")
  est_plotting_a <- read.table(file_name, col.names = labels)
  est_plotting_a <- est_plotting_a[seq_len(nrow(est_plotting_a)) %% 2 == 0,]
  est_plotting_a$subj_idx <- 1:99
  est_plotting_a$parameters <- "estimated"
  
  data_vline <- est_plotting_a %>% 
    dplyr::select(-c(likelihood, parameters,subj_idx)) %>% 
    pivot_longer(everything(),
                 names_to = "parameters",
                 values_to = "gen_estimates") %>% 
    group_by(parameters) %>% 
    summarise(vline_1 = mean(gen_estimates)) %>% 
    filter(parameters != "z.a" & parameters != "po")
  
  data_vline$vline_2  <- as.numeric(gen_plotting_data[1,data_vline$parameters])
  
  plt_dat <- est_plotting_a %>% 
    dplyr::select(-c(po, likelihood, parameters,subj_idx)) %>% 
    pivot_longer(everything(),
                 names_to = "parameters",
                 values_to = "gen_estimates")
  
  a <- ggplot(plt_dat,aes(x=gen_estimates)) + geom_histogram() + facet_wrap(~parameters,scales = "fixed")+
    geom_vline(data =data_vline, aes(xintercept = vline_1), color = "red")+
    geom_vline(data =data_vline, aes(xintercept = vline_2),color = "blue")+
    labs(title = paste0(data_vline$parameters, collapse = " "),
         subtitle = paste0(data_vline$vline_2, collapse = " "),
         caption = "red = mean, blue = generated")+ them
  b <- est_plotting_a %>% 
    dplyr::select(-c(po, likelihood, parameters,subj_idx)) %>% 
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
  
  # return(a + b)
  return(a)
  
}
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

#table one (parameter values for simulations)
load(here::here("scripts", "R", "exp_2_generated_sim_params.RData"))

generating_values <- as.data.frame(do.call(rbind, parameter_sets))
colnames(generating_values) <- c("speed", "acc", "t", "eta", "sa", "st", "v1", "v2", "v3", "v4")
generating_values$parameter_set <- letters[1:nrow(generating_values)]
# rownames(generating_values) <- letters[1:nrow(generating_values)]
library(gt)
generating_values[,c("parameter_set", "acc", "t", "eta", "sa", "st", "v1", "v2", "v3", "v4")] %>%  
  apa("Parameter Values for Simulations") %>%   cols_label(
    parameter_set = "Parameter set",
    acc = "a",
    t = "Ter",
  )

compare_parameters("~/Documents/hddm/scripts/fortran/res_a.txt", "a")
compare_parameters("~/Documents/hddm/scripts/fortran/res_b.txt", "b")
compare_parameters("~/Documents/hddm/scripts/fortran/res_c.txt", "c")
compare_parameters("~/Documents/hddm/scripts/fortran/res_d.txt", "d")
compare_parameters("~/Documents/hddm/scripts/fortran/res_e.txt", "e")
compare_parameters("~/Documents/hddm/scripts/fortran/res_f.txt", "f")
compare_parameters("~/Documents/hddm/scripts/fortran/res_g.txt", "g")
compare_parameters("~/Documents/hddm/scripts/fortran/res_h.txt", "h")
compare_parameters("~/Documents/hddm/scripts/fortran/res_i.txt", "i")
compare_parameters("~/Documents/hddm/scripts/fortran/res_j.txt", "j")
compare_parameters("~/Documents/hddm/scripts/fortran/res_k.txt", "k")
compare_parameters("~/Documents/hddm/scripts/fortran/res_l.txt", "l")

#pool and get mean and SD for each parameter
labels <- c("a_accuracy", "t", "eta", "sa", "z/a","st","po", "v_1", "v_2","v_3", "v_4", "likelihood")

est_data_a <- read.table("~/Documents/hddm/scripts/fortran/res_a.txt",col.names = labels)
est_data_a <- est_data_a[seq_len(nrow(est_data_a)) %% 2 == 0,]
est_data_a$parameter_set <- "a"

est_data_b <- read.table("~/Documents/hddm/scripts/fortran/res_b.txt",col.names = labels)
est_data_b <- est_data_b[seq_len(nrow(est_data_b)) %% 2 == 0,]
est_data_b$parameter_set <- "b"

est_data_c <- read.table("~/Documents/hddm/scripts/fortran/res_c.txt",col.names = labels)
est_data_c <- est_data_c[seq_len(nrow(est_data_c)) %% 2 == 0,]
est_data_c$parameter_set <- "c"

est_data_d <- read.table("~/Documents/hddm/scripts/fortran/res_d.txt",col.names = labels)
est_data_d <- est_data_d[seq_len(nrow(est_data_d)) %% 2 == 0,]
est_data_d$parameter_set <- "d"

est_data_e <- read.table("~/Documents/hddm/scripts/fortran/res_e.txt",col.names = labels)
est_data_e <- est_data_e[seq_len(nrow(est_data_e)) %% 2 == 0,]
est_data_e$parameter_set <- "e"

est_data_f <- read.table("~/Documents/hddm/scripts/fortran/res_f.txt",col.names = labels)
est_data_f <- est_data_f[seq_len(nrow(est_data_f)) %% 2 == 0,]
est_data_f$parameter_set <- "f"

est_data_g <- read.table("~/Documents/hddm/scripts/fortran/res_g.txt",col.names = labels)
est_data_g <- est_data_g[seq_len(nrow(est_data_g)) %% 2 == 0,]
est_data_g$parameter_set <- "g"

est_data_h <- read.table("~/Documents/hddm/scripts/fortran/res_h.txt",col.names = labels)
est_data_h <- est_data_h[seq_len(nrow(est_data_h)) %% 2 == 0,]
est_data_h$parameter_set <- "h"

est_data_i <- read.table("~/Documents/hddm/scripts/fortran/res_i.txt",col.names = labels)
est_data_i <- est_data_i[seq_len(nrow(est_data_i)) %% 2 == 0,]
est_data_i$parameter_set <- "i"

est_data_j <- read.table("~/Documents/hddm/scripts/fortran/res_j.txt",col.names = labels)
est_data_j <- est_data_j[seq_len(nrow(est_data_j)) %% 2 == 0,]
est_data_j$parameter_set <- "j"

est_data_k <- read.table("~/Documents/hddm/scripts/fortran/res_k.txt",col.names = labels)
est_data_k <- est_data_k[seq_len(nrow(est_data_k)) %% 2 == 0,]
est_data_k$parameter_set <- "k"

est_data_l <- read.table("~/Documents/hddm/scripts/fortran/res_l.txt",col.names = labels)
est_data_l <- est_data_l[seq_len(nrow(est_data_l)) %% 2 == 0,]
est_data_l$parameter_set <- "l"

est_data_all <- rbind(est_data_a,est_data_b,est_data_c, 
                      est_data_d,est_data_e, est_data_f,
                      est_data_g,est_data_h, est_data_i,
                      est_data_j,est_data_k,est_data_l
                      )

table_dat <- est_data_all %>% 
  group_by(parameter_set) %>% 
  dplyr::select(-c(po,likelihood)) %>% 
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
  
table_dat[order(table_dat$stats),]  %>%  group_by(stats) %>% apa(
  "Means and Standard Deviations of Parameter Values Recovered
From the SIMPLEX Fitting Method (N = 250 per Condition)"
) %>% 
  cols_label(
    stats = "Parameter set",
    a_accuracy = "a",
    t = "Ter",
  )

#correlation table
  #correlation plot
library(GGally)
plotdata <- est_data_all[,c("a_accuracy", "t", "eta", "sa","st", "v_1", "v_2","v_3", "v_4")]
p <- ggpairs(plotdata, title="") 
p+ them

modelsummary::datasummary_correlation(
  plotdata,
  output = "default",
  method = "pearson",
  fmt = 3,
  align = NULL,
  add_rows = NULL,
  add_columns = NULL,
  title = "Correlations Among Parameter Values for SIMPLEX Fits
(N = 250 per Condition)",
  notes = NULL,
  escape = TRUE,
)
