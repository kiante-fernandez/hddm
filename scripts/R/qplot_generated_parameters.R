library(purrr)
library(tidyverse)
#save the res for later reference (but we dont need to save everytime)
if (!file.exists(here::here("scripts", "R", "generated_sim_params.RData"))) {
  res <- generate_sa_simulations(genparam, sa, nt, ns)
  save(res, file = here::here("scripts", "R", "generated_sim_params.RData"))
}else {
  load(here::here("scripts", "R", "generated_sim_params.RData"))
}

#combine all data from generated for simulations fot plot 
# data = rbind(res[[1]][[1]],
#              res[[2]][[1]],
#              res[[3]][[1]],
#              res[[4]][[1]],
#              res[[5]][[1]])

data = rbind(res[[1]][[1]],
             res[[3]][[1]],
             res[[4]][[1]],
             res[[5]][[1]])

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
    legend.position = "right",
    # legend.position = "none",
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

# data %>%
#   mutate(response = as.integer(factor(response)) - 1) %>%
#   mutate(rtbin = round(rt,1)) %>% 
#   group_by(subject_idx, instructions, difficulty, sa, rtbin) %>%
#   summarise(acc = mean(response)) %>%
#   group_by(instructions, difficulty, sa, rtbin) %>%
#   summarise(mAcc = mean(acc)) %>% View

sim_acc <- data %>%
    mutate(response = as.integer(factor(response)) - 1) %>%
    group_by(subject_idx, instructions, difficulty, sa) %>%
    summarise(acc = mean(response)) %>%
    group_by(instructions, difficulty, sa) %>%
    summarise(mAcc = mean(acc))
  
sim_rt <- data %>%
    mutate(response = as.integer(factor(response)) - 1) %>%
    group_by(subject_idx, instructions, difficulty, response, sa) %>%
    summarise(
      rtQ1 = quantile(abs(rt),.1),
      rtQ3 = quantile(abs(rt),.3), 
      rtQ5 = quantile(abs(rt),.5),
      rtQ7 = quantile(abs(rt),.7),
      rtQ9 = quantile(abs(rt),.9)
    ) %>%
    group_by(difficulty, instructions, response, sa) %>%
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
  group_by(difficulty, instructions, response, sa) %>%
  mutate(acc = ifelse(response == 1,
                      sim_acc$mAcc[sim_acc$instructions == instructions &
                                     sim_acc$difficulty == difficulty &
                                     sim_acc$sa == sa],
                      1 - sim_acc$mAcc[sim_acc$instructions == instructions &
                                         sim_acc$difficulty == difficulty & 
                                         sim_acc$sa == sa]
    ))
  ggplot() +
    them+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q1, ymin = mRT_q1 - seRT_q1, ymax = mRT_q1 + seRT_q1, color = factor(sa), shape = factor(sa)),alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q3, ymin = mRT_q3 - seRT_q3, ymax = mRT_q3 + seRT_q3, color = factor(sa), shape = factor(sa)),alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q5, ymin = mRT_q5 - seRT_q5, ymax = mRT_q5 + seRT_q5, color = factor(sa), shape = factor(sa)),alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q7, ymin = mRT_q7 - seRT_q7, ymax = mRT_q7 + seRT_q7, color = factor(sa), shape = factor(sa)),alpha = .5)+
    geom_pointrange(data = obs, aes(x = acc, y = mRT_q9, ymin = mRT_q9 - seRT_q9, ymax = mRT_q9 + seRT_q9, color = factor(sa), shape = factor(sa)),alpha = .5)+
    geom_point(data = obs, aes(x = acc, y = mRT_q3, color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q5, color = factor(sa), shape = factor(sa)),  alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q7, color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_point(data = obs, aes(x = acc, y = mRT_q9, color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q1,  color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q3,  color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q5,  color = factor(sa), shape = factor(sa)),alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q7,  color = factor(sa), shape = factor(sa)), alpha = .5) +
    geom_line(data = obs, aes(x = acc, y = mRT_q9,  color = factor(sa), shape = factor(sa)), alpha = .5) +
    facet_wrap(~instructions)+
    labs(
      y = "RT(s)",
      x = "Response proportion",
      shape = "",
      color = "SA"
    )+
    scale_y_continuous(limits = c(.1, 1.5))+
    geom_hline(yintercept = 1.2, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = 1, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .8, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .6, color = "grey", linetype = "dashed")+
    geom_hline(yintercept = .4, color = "grey", linetype = "dashed")+
    scale_color_brewer(palette = "Set1")

  
#   
# labels <- c("a_acc","a_speed", "ter", "eta", "sa", "z/a","st","po", "v1", "v2","v3", "v4", "likelihood")
#   
# sim_res <- vector(mode = "list", length = 5)
#   # sa_idx = 5
# parameters <- read.table("~/Documents/hddm/scripts/fortran/fort.101", col.names = labels)
# nt = 300
# for (sa_idx in 1:5) {
# 
#     # create list for each of the conditions (instructions and diff)
#     temp_ds1 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds2 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds3 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds4 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds5 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds6 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds7 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     temp_ds8 <- vector(mode = "list", length = length(unique(parameters$subj_idx)))
#     # subj_idx = 1
#     for (subj_idx in seq_len(nrow(parameters))) {
#       # change sa parameter value
# 
#       a_speed_temp <- runif(nt, min = parameters$a_speed[[subj_idx]] -  parameters$sa[[subj_idx]], max = parameters$a_speed[[subj_idx]] +  parameters$sa[[subj_idx]])
#       
#       a_accuracy_temp <- runif(nt, min = parameters$a_acc[[subj_idx]] -  parameters$sa[[subj_idx]], max = parameters$a_acc[[subj_idx]] +  parameters$sa[[subj_idx]])
#       
#       # speed
#       temp_ds1[subj_idx] <- list(map_df(a_speed_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v1[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$ter[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds2[subj_idx] <- list(map_df(a_speed_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v2[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$ter[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds3[subj_idx] <- list(map_df(a_speed_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v3[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$ter[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds4[subj_idx] <- list(map_df(a_speed_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v4[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$ter[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       # accuracy
#       temp_ds5[subj_idx] <- list(map_df(a_accuracy_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v1[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$t[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds6[subj_idx] <- list(map_df(a_accuracy_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v2[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$t[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds7[subj_idx] <- list(map_df(a_accuracy_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v3[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$t[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       temp_ds8[subj_idx] <- list(map_df(a_accuracy_temp,function (.){rtdists::rdiffusion(n = 1, a = ., v = parameters$v4[[subj_idx]], sv = parameters$eta[[subj_idx]], t0 = parameters$t[[subj_idx]], st0 = parameters$st[[subj_idx]], z = parameters$z.a[[subj_idx]] * ., s = 0.1)}))
#       
#     }
#     ds <- list(temp_ds1, temp_ds2, temp_ds3, temp_ds4, temp_ds5, temp_ds6, temp_ds7, temp_ds8)
#     # clean up the datasets
#     org_res <- vector(mode = "list", length = length(ds))
#     for (ds_idx in 1:8) {
#       temp_dat <- data.frame(subject_idx = unlist(purrr::map(seq(1, ns), rep, times = nt)), do.call(rbind, ds[[ds_idx]]))
#       
#       if (ds_idx < 5) {
#         temp_dat$instructions <- "speed"
#       } else {
#         temp_dat$instructions <- "accuracy"
#       }
#       
#       if (ds_idx %in% c(1, 5)) {
#         temp_dat$difficulty <- 1
#       } else if (ds_idx %in% c(2, 6)) {
#         temp_dat$difficulty <- 2
#       } else if (ds_idx %in% c(3, 7)) {
#         temp_dat$difficulty <- 3
#       } else {
#         temp_dat$difficulty <- 4
#       }
#       temp_dat$sa <- sa[[sa_idx]]
#       org_res[[ds_idx]] <- temp_dat
#     }
#     
#     sim_res[[sa_idx]] <- list(dataset = do.call(rbind, org_res), parameters = parameters)
#   }
