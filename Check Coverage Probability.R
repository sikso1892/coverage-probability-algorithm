# -- setting -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(xlsx)
library(openxlsx)
library(data.table)
library(ggplot2)
library(dplyr)

setwd("/home/rstudio/work")

source("Coverage Probability Algorithm.R")
source("Calculate CI.R")
# Simon, Shan, Our Sample size
data_file = "Result_p_0_p_1_0.15.xlsx"

# alpha = 0.05/ beta = 0.1
alpha = 0.05; beta = 0.1

Sample1 = read_excel(data_file, sheet = "alpha=0.05 beta = 0.1", range = "A1:O40") %>% as.data.table()
names(Sample1)[6:15] = c(paste0("Simon_", c("n_1", "r_1", "n_2", "r")),
                         paste0("Shan_",  c("n_2_new", "n_new", "r_new")),
                         paste0("Ours_",  c("n_2_new", "n_new", "r_new")))

Sample1 = Sample1[-1, ]


################################################################################
# -- p_0 = 0.6, p_1 = 0.8, alpha = 0.05, beta = 0.1 ---------------------------
# 
p_0_0.6 = Sample1[p_0 == 0.6]
# data = p_0_0.5
# data = data_Simon1
# Calculate CI
CI_p_0_0.6 = Calculate.CI(p_0_0.6)

# data = p_0_0.1
CP_p_0_0.6 = Calculate.CP(CI_p_0_0.6)

################################################################################
# -- p_0 = 0.2, p_1 = 0.4, alpha = 0.05, beta = 0.1 ---------------------------

p_0_0.2 = Sample1[p_0 == 0.2]
# data = p_0_0.5
# data = data_Simon1
# Calculate CI
CI_p_0_0.2 = Calculate.CI(p_0_0.2)

# data = p_0_0.1
CP_p_0_0.2 = Calculate.CP(CI_p_0_0.2)

################################################################################
# -- p_0 = 0.3, p_1 = 0.5, alpha = 0.05, beta = 0.1 ---------------------------

p_0_0.3 = Sample1[p_0 == 0.3]
# data = p_0_0.5
# data = data_Simon1
# Calculate CI
CI_p_0_0.3 = Calculate.CI(p_0_0.3)

# data = p_0_0.1
CP_p_0_0.3 = Calculate.CP(CI_p_0_0.3)