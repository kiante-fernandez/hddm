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
# 2023/01/07    Kianté Fernandez                      wrote orignal code

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
# sa <- c(0, .001, .01, .05, .075, 0.1)
sa <- c(0, .001, .01, .05, .07)


nt <- 1200 # number of trials
#varying across a number of trials later
ns <- 10 # number of subjects

res <- generate_sa_simulations(genparam, sa, nt, ns)

##Prepare data for fortran fitting
for (condition_idx in c("SPEED","ACCURACY")){
  for (sa_idx in 1:length(sa)){
    prepare_fortran(res, condition_idx, sa_idx)
  }
}

#TODO can you pull in the results from the FORTRAN code ? Yes. see the fort. file for those you can make a table of them

# you need to change nfile in the FORTRAN code to load each of the res

labels <- c("a", "ter", "eta", "sa", "z/a","st","po", "v1", "v2","v3", "v4", "likelihood")
#speed
res_1 <- read.table("~/Documents/hddm/scripts/fortran/fort.101",col.names = labels)
res_2 <- read.table("~/Documents/hddm/scripts/fortran/fort.102",col.names = labels)
res_3 <- read.table("~/Documents/hddm/scripts/fortran/fort.103",col.names = labels)
res_4 <- read.table("~/Documents/hddm/scripts/fortran/fort.104",col.names = labels)
res_5 <- read.table("~/Documents/hddm/scripts/fortran/fort.105",col.names = labels)
#acc
res_6 <- read.table("~/Documents/hddm/scripts/fortran/fort.106",col.names = labels)
res_7 <- read.table("~/Documents/hddm/scripts/fortran/fort.107",col.names = labels)
res_8 <- read.table("~/Documents/hddm/scripts/fortran/fort.108",col.names = labels)
res_9 <- read.table("~/Documents/hddm/scripts/fortran/fort.109",col.names = labels)
res_10 <- read.table("~/Documents/hddm/scripts/fortran/fort.110",col.names = labels)

res[[1]][["parameters"]]
res_1$sa
res_2$sa
res_3$sa
res_4$sa
res_5$sa
res_6$sa
res_7$sa
res_8$sa
res_9$sa
#0.116   0.414   0.048   0.027   0.607   0.100   0.288   0.413   0.163   0.103  -0.271     -436.63208


#ACCURACY

# 0.095   0.479   0.057   0.031   0.155   0.104   0.144   0.345   0.213   0.101  -0.326    -1287.68341
# 0.163   0.434   0.047   0.019   0.858   0.050   0.286   0.396   0.180   0.087  -0.369       67.30284
# 0.085   0.401   0.084   0.021   0.137   0.102   0.088   0.338   0.212   0.083  -0.309    -1327.03205
# 0.095   0.448   0.043   0.034   0.213   0.224   0.118   0.421   0.164   0.070  -0.387     -399.80293
# 0.106   0.218   0.041   0.027   0.199   0.146   0.169   0.359   0.206   0.099  -0.414     -812.18337
# 0.113   0.428   0.037   0.030   0.285   0.271   0.165   0.149   0.126   0.070  -0.269     -222.66917
# 0.140   0.528   0.034   0.029   0.518   0.188   0.224   0.391   0.166   0.104  -0.407      -21.89227
# 0.085   0.506   0.061   0.026   0.268   0.109   0.173   0.380   0.263   0.152  -0.374     -781.45276
# 0.138   0.256   0.056   0.027   0.835   0.054   0.305   0.488   0.268   0.057  -0.212      -92.98502

#SPEED

# 0.132   0.422   0.077   0.028   0.541   0.069   0.008   0.404   0.136   0.101  -0.330    -1066.78886
# 0.065   0.424   0.123   0.032   0.651   0.040   0.001   0.548   0.152   0.089  -0.331    -1872.72960
# 0.130   0.371   0.099   0.033   0.484   0.082   0.001   0.413   0.283   0.086  -0.523    -1094.52739
# 0.115   0.296   0.097   0.028   0.536   0.061   0.003   0.388   0.159   0.081  -0.313    -1006.00140
# 0.138   0.113   0.069   0.027   0.472   0.039   0.002   0.365   0.134   0.119  -0.340    -1095.90687
# 0.116   0.204   0.089   0.028   0.499   0.064   0.000   0.365   0.114   0.087  -0.425    -1030.06011
# 0.118   0.428   0.091   0.030   0.504   0.078   0.003   0.311   0.237   0.097  -0.300     -780.51431
# 0.088   0.454   0.121   0.031   0.446   0.053   0.007   0.407   0.199   0.157  -0.371    -1394.75846
# 0.065   0.238   0.245   0.024   0.543   0.030   0.000   0.458   0.205   0.103  -0.387    -1729.36041

#could you load it just as a text file
labels <- c("a", "ter", "eta", "sa", "z","st","po", "v1", "v2","v3", "v4", "likelihood")

#sa = 0
# a   ter   eta    sa     z    st    po    v1    v2    v3     v4 likelihood
# 1 0.097 0.242 0.097 0.027 0.536 0.040 0.008 0.404 0.161 0.071 -0.212  -1331.930
# 2 0.086 0.248 0.080 0.024 0.493 0.043 0.004 0.343 0.149 0.086 -0.350  -1316.451
# 3 0.091 0.261 0.136 0.035 0.464 0.049 0.001 0.398 0.228 0.090 -0.339  -1313.013
# 4 0.092 0.370 0.102 0.028 0.498 0.061 0.000 0.364 0.205 0.089 -0.410  -1176.229
# 5 0.092 0.382 0.089 0.028 0.473 0.036 0.002 0.370 0.149 0.115 -0.296  -1357.781
# 6 0.083 0.425 0.141 0.030 0.510 0.057 0.000 0.403 0.169 0.085 -0.420  -1333.565
# 7 0.088 0.396 0.086 0.032 0.447 0.055 0.003 0.315 0.196 0.102 -0.345  -1227.007
# 8 0.090 0.409 0.114 0.022 0.455 0.049 0.007 0.386 0.271 0.121 -0.346  -1310.603
# 9 0.093 0.142 0.110 0.029 0.508 0.045 0.000 0.400 0.322 0.053 -0.290  -1274.769

# sa = .5
# a   ter   eta    sa     z    st    po    v1    v2    v3     v4 likelihood
# 1 0.065 0.228 0.103 0.039 0.594 0.030 0.000 0.530 0.156 0.070 -0.298 -1933.6590
# 2 0.067 0.421 0.111 0.038 0.553 0.047 0.001 0.424 0.174 0.098 -0.309 -1592.1132
# 3 0.065 0.124 0.188 0.028 0.530 0.030 0.001 0.495 0.281 0.102 -0.305 -1887.9359
# 4 0.065 0.305 0.120 0.020 0.518 0.037 0.000 0.402 0.219 0.102 -0.389 -1637.0027
# 5 0.133 0.217 0.065 0.034 0.486 0.044 0.002 0.308 0.139 0.119 -0.305  -819.2476
# 6 0.086 0.285 0.130 0.024 0.562 0.044 0.000 0.418 0.148 0.098 -0.357 -1297.3007
# 7 0.119 0.352 0.073 0.035 0.518 0.069 0.003 0.353 0.256 0.100 -0.377 -1015.7284
# 8 0.065 0.225 0.228 0.028 0.570 0.030 0.004 0.516 0.328 0.142 -0.314 -1866.9085
# 9 0.075 0.329 0.143 0.030 0.489 0.039 0.000 0.376 0.270 0.080 -0.293 -1431.4110


res_2 <- read.table("~/Documents/hddm/scripts/fortran/fort.122",col.names = labels)


