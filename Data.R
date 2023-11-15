# -- setting -------------------------------------------------------------------
rm(list = ls())

library(readxl)
library(xlsx)
library(openxlsx)
library(data.table)
library(ggplot2)
library(dplyr)

source("D:/Sungah/성균관대학교/논문/Coverage Probability Algorithm(Final).R")
source("D:/Sungah/성균관대학교/논문/Calculate CI.R")
# Simon, Shan, Our Sample size
data_file = "D:/Sungah/성균관대학교/논문/Result_p_0_p_1_0.15.xlsx"

# alpha = 0.05/ beta = 0.1
alpha = 0.05; beta = 0.1

Sample1 = read_excel(data_file, sheet = "alpha=0.05 beta = 0.1", range = "A1:O50") %>% as.data.table()
names(Sample1)[6:15] = c(paste0("Simon_", c("n_1", "r_1", "n_2", "r")),
                         paste0("Shan_",  c("n_2_new", "n_new", "r_new")),
                         paste0("Ours_",  c("n_2_new", "n_new", "r_new")))

Sample1 = Sample1[-1, ]


################################################################################
# -- p_0 = 0.5, p_1 = 0.65, alpha = 0.05, beta = 0.1 ---------------------------

p_0_0.5 = Sample1[p_0 == 0.5]
# data = p_0_0.5
# data = data_Simon1
# Calculate CI
CI_p_0_0.5 = Calculate.CI(p_0_0.5)

# data = p_0_0.1
CP_p_0_0.5 = Calculate.CP(CI_p_0_0.5)

Plot_p_0_0.5 = CP_p_0_0.5 %>%
  ggplot(aes(pi_0, coverage.probability)) +
  geom_line(aes(linetype = Design, color = Design)) +
  # geom_point() +
  geom_hline(yintercept = 0.95, linetype = 'dashed', color = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0.9, 1)) +
  theme_bw() +
  scale_color_manual(values = c("Simon" = "black", "Shan" = "red", "Ours" = "blue"))  # 색상 지정

ggsave(Plot_p_0_0.5, "D:/Sungah/성균관대학교/논문/Plot of p_0 0.png", device = "png",
       width = 10, height = 7)

################################################################################
# -- p_0 = 0.4, p_1 = 0.55, alpha = 0.05, beta = 0.1 ---------------------------

p_0_0.4 = Sample1[p_0 == 0.4]

# data = p_0_0.1
pi = 0.4
CI_p_0_0.4 = Calculate.CI(p_0_0.4)

# data = p_0_0.1
CP_p_0_0.4 = Calculate.CP(CI_p_0_0.4)

Plot_p_0_0.4 = CP_p_0_0.4 %>%
  ggplot(aes(pi_0, coverage.probability)) +
  geom_line(aes(linetype = Design, color = Design), linewidth = 1.5) +
  # geom_point() +
  geom_hline(yintercept = 0.95, linetype = 'dashed', color = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0.9, 1)) +
  theme_bw() +
  scale_color_manual(values = c("Simon" = "black", "Shan" = "red", "Ours" = "blue"))  # 색상 지정

# ggsave(Plot_p_0_0.4, "D:/KSA/공부/대학원/논문/Simon two stage/Simon_Shan_결과/Coverage Probability/Simon Shan Ours Plot 비교/Plot of p_0 0.4.png", device = "png",
#        width = 10, height = 7)

################################################################################
# -- p_0 = 0.3, p_1 = 0.55, alpha = 0.05, beta = 0.1 ---------------------------

p_0_0.3 = Sample1[p_0 == 0.3]

# data = p_0_0.1
pi = 0.3
CI_p_0_0.3 = Calculate.CI(p_0_0.3)

# data = p_0_0.1
CP_p_0_0.3 = Calculate.CP(CI_p_0_0.3)

Plot_p_0_0.3 = CP_p_0_0.3 %>%
  ggplot(aes(pi_0, coverage.probability)) +
  geom_line(aes(linetype = Design, color = Design), linewidth = 1.5) +
  # geom_point() +
  geom_hline(yintercept = 0.95, linetype = 'dashed', color = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0.9, 1)) +
  theme_bw() +
  scale_color_manual(values = c("Simon" = "black", "Shan" = "red", "Ours" = "blue"))  # 색상 지정


ggsave(Plot_p_0_0.3, "D:/KSA/공부/대학원/논문/Simon two stage/Simon_Shan_결과/Coverage Probability/Simon Shan Ours Plot 비교/Plot of p_0 0.3.png", device = "png",
       width = 10, height = 7)


################################################################################
# -- p_0 = 0.2, p_1 = 0.4, alpha = 0.05, beta = 0.2 ---------------------------