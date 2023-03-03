# qplot.R - plots choice and response time data
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
# 2023/02/22    Kianté Fernandez                      wrote code

library(tidyverse)
library(patchwork)

qplot <- function(data) {
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
  sim_acc <- data %>%
    mutate(response = as.integer(factor(response)) - 1) %>%
    group_by(subject_idx, instructions, difficulty) %>%
    summarise(acc = mean(response)) %>%
    group_by(instructions, difficulty) %>%
    summarise(mAcc = mean(acc))

  sim_rt <- data %>%
    mutate(response = as.integer(factor(response)) - 1) %>%
    group_by(subject_idx, instructions, difficulty, response) %>%
    summarise(
      rtQ1 = quantile(abs(rt),.1),
      rtQ3 = quantile(abs(rt),.3), 
      rtQ5 = quantile(abs(rt),.5),
      rtQ7 = quantile(abs(rt),.7),
      rtQ9 = quantile(abs(rt),.9)
    ) %>%
    group_by(difficulty, instructions, response) %>%
    summarise(
      mRT_q1 = median(rtQ1),
      seRT_q1 = sqrt(var(rtQ1) / length(rtQ1)),
      mRT_q3 = median(rtQ3),
      seRT_q3 = sqrt(var(rtQ3) / length(rtQ3)),
      mRT_q5 = median(rtQ5),
      seRT_q5 = sqrt(var(rtQ5) / length(rtQ5)),
      mRT_q7 = median(rtQ7),
      seRT_q7 = sqrt(var(rtQ7) / length(rtQ7)),
      mRT_q9 = median(rtQ9),
      seRT_q9 = sqrt(var(rtQ9) / length(rtQ9))
    )

  obs <- sim_rt %>%
    group_by(difficulty, instructions, response) %>%
    mutate(acc = ifelse(response == 1,
                        sim_acc$mAcc[sim_acc$instructions == instructions &
                                       sim_acc$difficulty == difficulty],
                        1 - sim_acc$mAcc[sim_acc$instructions == instructions &
                                           sim_acc$difficulty == difficulty]
    ))
  ggplot() +
    them +
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q1, ymin = mRT_q1 - seRT_q1, ymax = mRT_q1 + seRT_q1, shape = instructions),color = "blue",   alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q3, ymin = mRT_q3 - seRT_q3, ymax = mRT_q3 + seRT_q3, shape = instructions),color = "orange", alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q5, ymin = mRT_q5 - seRT_q5, ymax = mRT_q5 + seRT_q5, shape = instructions),color = "red",    alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q7, ymin = mRT_q7 - seRT_q7, ymax = mRT_q7 + seRT_q7, shape = instructions),color = "purple", alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q9, ymin = mRT_q9 - seRT_q9, ymax = mRT_q9 + seRT_q9, shape = instructions),color = "green",  alpha = .5)+
    geom_point(data = obs, aes(x = acc, y = mRT_q1, shape = instructions), color = "blue", alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q3, shape = instructions), color = "orange", alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q5, shape = instructions), color = "red", alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q7, shape = instructions), color = "purple", alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q9, shape = instructions), color = "green", alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q1, shape = instructions), color = "blue", alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q3, shape = instructions), color = "orange", alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q5, shape = instructions), color = "red", alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q7, shape = instructions), color = "purple", alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q9, shape = instructions), color = "green", alpha = .5) +
    labs(
      y = "RT(s)",
      x = "Response proportion",
      shape = ""
    ) +
    facet_wrap(~instructions)+
    scale_y_continuous(limits = c(.1, 1.5))+
    geom_hline(yintercept = 1.2, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = 1, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .8, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .6, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .4, color = "grey", linetype = "dashed")
  
}



p1 <- qplot(res[[1]][[1]])
p2 <- qplot(res[[2]][[1]])
p3 <- qplot(res[[3]][[1]])
p4 <- qplot(res[[4]][[1]])
p5 <- qplot(res[[5]][[1]])

(p1 + p2 + p3) / (p4 + p5) +
  plot_annotation("across trial variability in boundary separation", tag_levels = "A")

p1 + p5

#######
#fitting parameter revocer plots

if (!file.exists(here::here("scripts", "R", "generated_sim_params.RData"))) {
  res <- generate_sa_simulations(genparam, sa, nt, ns)
  save(res, file = here::here("scripts", "R", "generated_sim_params.RData"))
}else {
  load(here::here("scripts", "R", "generated_sim_params.RData"))
}

#while the models are running I can just add  noise and make the plots
gen_plotting_data <- res[[1]][[2]]
labels <- c("a_accuracy","a_speed", "t", "eta", "sa", "z/a","st", "v_1", "v_2","v_3", "v_4", "likelihood")
est_plotting_data <- read.table("~/Documents/hddm/scripts/fortran/fort.300", col.names = labels)
# est_plotting_data <- read.table("~/Documents/hddm/scripts/fortran/fort.300",col.names = labels)

# plot_dat <- data_frame(gen_a_speed = plotting_data$a_speed,
#            est_a_speed = (plotting_data$a_speed + rnorm(50, 0,.002))
#            )

sa_hist1 <- ggplot(est_plotting_data, aes(sa))+
  geom_histogram(fill = "white", color = "black", bins = 35)+
  scale_x_continuous(limits = c(0.00,0.20)) +
  them+ 
  labs(title= "SA = 0")+
  geom_vline(xintercept = 0.0, color = "red")+
  geom_vline(xintercept = 0.03, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$sa), color = "blue")
  

eta_hist1 <- ggplot(est_plotting_data, aes(eta))+
  geom_histogram(fill = "white", color = "black", bins = 35) + 
  scale_x_continuous(limits = c(0.00,0.40)) +
  them + 
  labs(title= "Eta = 0.1040")+
  geom_vline(xintercept = 0.1040, color = "red")+
  geom_vline(xintercept = 0.13, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$eta), color = "blue")

psych::describe(est_plotting_data$sa)

gen_plotting_data <- gen_plotting_data %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "gen_estimates") 

plot_dat <- est_plotting_data %>% 
  select(a_accuracy,a_speed, t,v_1,v_2,v_3,v_4) %>% 
  mutate(subj_idx = seq_len(nrow(est_plotting_data))) %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "est_estimates") %>% 
  left_join(gen_plotting_data)

plot_dat %>% 
  select(-subj_idx) %>% 
  group_by(parameters) %>% 
  correlation::correlation()

sa1 <- ggplot()+
  geom_point(data = plot_dat, aes(x = gen_estimates, y = est_estimates), color = "black")+
  geom_abline(intercept = 0, slope = 1, color = "gray")+
  labs(
    x = "estimand",
    y = "estimate") +
  facet_wrap(~parameters, scales = "free")+ them

# gen_plotting_data <- res[[3]][[2]]
# 
# gen_plotting_data <- gen_plotting_data %>% 
#   pivot_longer(-subj_idx, 
#                names_to = "parameters",
#                values_to = "gen_estimates") 
# 
# est_plotting_data <- read.table("~/Documents/hddm/scripts/fortran/fort.301",col.names = labels)
# 
# sa_hist3 <- ggplot(est_plotting_data, aes(sa))+
#   geom_histogram(fill = "white", color = "black", bins = 35) + 
#   scale_x_continuous(limits = c(0.00,0.15)) + them + 
#   labs(title= "SA = 0.01")+
#   geom_vline(xintercept = 0.01, color = "red")
# 
# psych::describe(est_plotting_data$sa)
# 
# plot_dat <- est_plotting_data %>% 
#   select(a_accuracy,a_speed, t,v_1,v_2,v_3,v_4) %>% 
#   mutate(subj_idx = seq_len(nrow(est_plotting_data))) %>% 
#   pivot_longer(-subj_idx, 
#                names_to = "parameters",
#                values_to = "est_estimates") %>% 
#   left_join(gen_plotting_data)
# 
# plot_dat %>% 
#   select(-subj_idx) %>% 
#   group_by(parameters) %>% 
#   correlation::correlation()
# 
# sa3 <- ggplot()+
#   geom_point(data = plot_dat, aes(x = gen_estimates, y = est_estimates), color = "black")+
#   geom_abline(intercept = 0, slope = 1, color = "gray")+
#   labs(
#     x = "estimand",
#     y = "estimate") +
#   facet_wrap(~parameters, scales = "free")+ them
# 
# sa1 +  sa3
# 
# sa_hist1 /sa_hist3
########

gen_plotting_data <- res[[5]][[2]]


gen_plotting_data <- gen_plotting_data %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "gen_estimates") 

est_plotting_data <- read.table("~/Documents/hddm/scripts/fortran/fort.304",col.names = labels)

sa_hist5 <- ggplot(est_plotting_data, aes(sa))+
  geom_histogram(fill = "white", color = "black", bins = 35) + 
  scale_x_continuous(limits = c(0.00,0.20)) +
  them + 
  labs(title= "SA = 0.07")+
  geom_vline(xintercept = 0.07, color = "red")+
  geom_vline(xintercept = 0.03, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$sa), color = "blue")

eta_hist5 <- ggplot(est_plotting_data, aes(eta))+
  geom_histogram(fill = "white", color = "black", bins = 35) + 
  scale_x_continuous(limits = c(0.00,0.40)) +
  them + 
  labs(title= "Eta = 0.1040")+
  geom_vline(xintercept = 0.1040, color = "red")+
  geom_vline(xintercept = 0.13, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$eta), color = "blue")

psych::describe(est_plotting_data$sa)

plot_dat <- est_plotting_data %>% 
  select(a_accuracy,a_speed, t,v_1,v_2,v_3,v_4) %>% 
  mutate(subj_idx = seq_len(nrow(est_plotting_data))) %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "est_estimates") %>% 
  left_join(gen_plotting_data)

plot_dat %>% 
  select(-subj_idx) %>% 
  group_by(parameters) %>% 
  correlation::correlation()

sa5 <- ggplot()+
  geom_point(data = plot_dat, aes(x = gen_estimates, y = est_estimates), color = "black")+
  geom_abline(intercept = 0, slope = 1, color = "gray")+
  labs(
    x = "estimand",
    y = "estimate") +
  facet_wrap(~parameters, scales = "free")+ them

sa1 + sa5

sa_hist1 / sa_hist5
eta_hist1/ eta_hist5




gen_plotting_data <- res[[1]][[2]]

gen_plotting_data <- gen_plotting_data %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "gen_estimates") 

labels <- c("a_accuracy","a_speed", "t", "eta", "sa","st", "v_1", "v_2","v_3", "v_4", "likelihood")
est_plotting_data <- read.table("~/Documents/hddm/scripts/fortran/fort.200",col.names = labels)

sz_hist1 <- ggplot(est_plotting_data, aes(sa))+
  geom_histogram(fill = "white", color = "black", bins = 35) + 
  scale_x_continuous(limits = c(0.00,0.20)) +
  them + 
  labs(title= "Estimate SZ, SA = 0.00,", x = "sz")+
  geom_vline(xintercept = 0.07, color = "red")+
  geom_vline(xintercept = 0.03, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$sa), color = "blue")

eta_hist1 <- ggplot(est_plotting_data, aes(eta))+
  geom_histogram(fill = "white", color = "black", bins = 35) + 
  scale_x_continuous(limits = c(0.00,0.40)) +
  them + 
  labs(title= "Eta = 0.1040")+
  geom_vline(xintercept = 0.1040, color = "red")+
  geom_vline(xintercept = 0.13, color = "orange")+
  geom_vline(xintercept = median(est_plotting_data$eta), color = "blue")

psych::describe(est_plotting_data$sa)

plot_dat <- est_plotting_data %>% 
  select(a_accuracy,a_speed, t,v_1,v_2,v_3,v_4) %>% 
  mutate(subj_idx = seq_len(nrow(est_plotting_data))) %>% 
  pivot_longer(-subj_idx, 
               names_to = "parameters",
               values_to = "est_estimates") %>% 
  left_join(gen_plotting_data)

plot_dat %>% 
  select(-subj_idx) %>% 
  group_by(parameters) %>% 
  correlation::correlation()

saz <- ggplot()+
  geom_point(data = plot_dat, aes(x = gen_estimates, y = est_estimates), color = "black")+
  geom_abline(intercept = 0, slope = 1, color = "gray")+
  labs(title = "estimate SZ with SA = 0.00",
    x = "estimand",
    y = "estimate") +
  facet_wrap(~parameters, scales = "free")+ them


res[[5]][[2]]

gen_plotting_data <- res[[1]][[2]]
gen_plotting_data$sa <- 0.00
gen_plotting_data$parameters <- "generated"

labels <- c("a_accuracy","a_speed", "t", "eta", "sa", "z/a","st", "v_1", "v_2","v_3", "v_4", "likelihood")
est_plotting_datasa1 <- read.table("~/Documents/hddm/scripts/fortran/fort.100", col.names = labels)
est_plotting_datasa5 <- read.table("~/Documents/hddm/scripts/fortran/fort.104", col.names = labels)
est_plotting_datasa1$parameters <- "estimated"
est_plotting_datasa5$parameters <- "estimated"

knitr::kable(rbind(gen_plotting_data[,-1],
                   est_plotting_datasa1[,dput(names(gen_plotting_data[,-1]))],
                   est_plotting_datasa5[,dput(names(gen_plotting_data[,-1]))]), digits = 3)



write.csv(res[[1]][[2]], "~/Documents/hddm/scripts/R/generated_parameters.csv") 
