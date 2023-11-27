# -- setting -------------------------------------------------------------------
# -- Optimal Design ------------------------------------------------------------
# -- p1 - p0 = 0.2 -------------------------------------------------------------
rm(list = ls())

library(foreach)
library(doParallel)
library(readxl)
library(xlsx)
library(openxlsx)
library(data.table)
library(ggplot2)
library(dplyr)
library(pbapply)

source("Promising zone in 3 stage.R")

data_file = "3Stage_Data.xlsx"

sheet_name = "Page 6-7"

data = readxl::read_excel(data_file, sheet = sheet_name, range = "A4:H49") %>% as.data.table

names(data) = c("p_0", "p_1", "stage1", "stage2", "stage3", "EN", "PET_s1", "PET_all")

data$p0 = rep(seq(0.05, 0.75, by = 0.05), each = 3)

data[, p1 := p0 + 0.2]

data = data %>% select(-p_0, -p_1)

data$alpha = rep(c(0.1, 0.05, 0.05), times = length(data$p0 %>% unique))
data$beta  = rep(c(0.1, 0.2,  0.1),  times = length(data$p0 %>% unique))

data[, c('r_1', 'n_1') := tstrsplit(stage1, '/', type.convert = TRUE)]
data[, c('r_2', 'n_1_n_2') := tstrsplit(stage2, '/', type.convert = TRUE)]
data[, c('r_3', 'n_1_n_2_n_3') := tstrsplit(stage3, '/', type.convert = TRUE)]

data[, n_2 := n_1_n_2 - n_1]
data[, n_3 := n_1_n_2_n_3 - n_1_n_2]

data[, r_1 := r_1 + 1]
data[, r_2 := r_2 + 1]
data[, r_3 := r_3 + 1]

dat = data %>% select(-stage1, -stage2, -stage3, -n_1_n_2, -n_1_n_2_n_3)

# data = data[1, ]

# CP_0_value 값 설정
CP_0_value = 0.05 # 예시 값

# 시간 측정 시작
start_time <- Sys.time()

# 각 행에 대해 Find_SS_Promising_ft 함수를 적용
results <- pblapply(1:nrow(dat), function(i) {
  Find_SS_Promising_ft(dat[i, ], CP_0_value)
})

# 결과 확인
results = rbindlist(results, fill = T)
# 시간 측정 종료
end_time <- Sys.time()

# 걸린 시간 출력
time_taken <- end_time - start_time
print(time_taken)