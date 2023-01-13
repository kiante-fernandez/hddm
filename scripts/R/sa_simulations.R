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
set.seed(555)

### Simulations ###

# Mean parameters taken from Young adults from Experiment 2 in:
# Ratcliff, R., Thapar, A., & McKoon, G. (2001).
# The effects of aging on reaction time in a signal detection task.
# Psychology and Aging, 16(2), 323–341. https://doi.org/10.1037/0882-7974.16.2.323

# create a list with the population level parameters
genparam <- vector(mode = "list")

# Group-level means
genparam$a_speed_mu <- 0.91
genparam$a_accuracy_mu <- 1.48
genparam$v_1_mu <- 3.30
genparam$v_2_mu <- 2.05
genparam$v_3_mu <- 1.08
genparam$v_4_mu <- 0.43
genparam$t_mu <- 0.28
# Group-level variability
genparam$t_sd <- 0.175
genparam$v_sd <- 0.25 # this is our `eta` for our purposes

# select a range of sa values
sa <- c(0, .01, .1, .5, .75)
nt <- 1200 # number of trials
ns <- 10 # number of subjects

res <- generate_sa_simulations(genparam, sa, nt, ns)

##Prepare data for fortran fitting

condition <- "ACCURACY"
sa_condition <- 1

prepare_fortran(res, condition, sa_condition)


#TODO can you pull in the results from the FORTRAN code ? Yes. see the fort. file for those you can make a table of them


#0.116   0.414   0.048   0.027   0.607   0.100   0.288   0.413   0.163   0.103  -0.271     -436.63208

