# -- setting -------------------------------------------------------------------
library(foreach)
library(data.table)
library(dplyr)

# X1 > r1 : proceeds to the second stage(X_1 <= r_1 : 조기중단)
# X1 + X2 > r : H0 reject

################################################################################
# -- CP function for Simon Design ----------------------------------------------
CI.ft.Simon = function(data){
  
  n_1 = data$n_1[1]
  r_1 = data$r_1[1]
  n_2 = data$n_2[1]
  
  # ----------------------------------------------------------------------------
  # generate x_1
  data = data[, .(x_1 = seq(0, n_1, by = 1)), by = .(n_1, n_2, r_1, r, n_2_new)]
  
  # ----------------------------------------------------------------------------
  # generate x_2 by x_1 without using foreach
  # for x_1 <= r_1, x_2 is NA
  data[x_1 <= r_1, x_2 := NA_integer_]
  
  # x_1 > r_1인 경우 각 조합에 대해 x_2의 시퀀스를 생성합니다.
  data_x2 <- data[x_1 > r_1, .(x_2 = seq(0, n_2, by = 1)), by = .(x_1, n_1, n_2, r_1, r, n_2_new)]
  
  # 데이터를 결합합니다: NA를 포함한 원본 데이터와 새로운 x_2 시퀀스가 있는 데이터
  data <- rbind(data[x_1 <= r_1], data_x2, fill = TRUE)
  
  
  # ----------------------------------------------------------------------------
  # calculate stage1
  data_stage1 = data[is.na(x_2)]
  data_stage1[, pvalue := ifelse(x_1 == 0, 1, pbinom((x_1-1), n_1, pi, lower.tail = F))]
  
  data_stage1 = data_stage1[, .(x_1, x_2, pvalue)]
  
  # ----------------------------------------------------------------------------
  # calculate stage2
  data_stage2 = data[!is.na(x_2)]
  
  data_stage2_1 = foreach(i = 1 : length(data_stage2$x_1)) %do% {
    
    temp3 = data_stage2[i, ]
    
    temp3_1 = temp3 
    
    stage2 = temp3_1[, .(a = seq(temp3$x_1, n_1, by = 1)), by = .(n_1, n_2, r_1, r, n_2_new, x_1, x_2)]
    stage2 = stage2[, x_1_x_2_a := x_1 + x_2 - a]
    
    stage2_final = stage2
    stage2_final[, x_1_x_2_a:= ifelse(x_1_x_2_a <0, 0, x_1_x_2_a)]
    
    stage2_final = stage2_final[, dbin := dbinom(a, n_1, pi)]
    stage2_final = stage2_final[, pbin := pbinom((x_1_x_2_a - 1), n_2, pi, lower.tail = F)]
    stage2_final = stage2_final[, dbin_pbin :=  dbin * pbin]
    
    pvalue = sum(stage2_final$dbin_pbin)
    
    return(data.table(x_1 = temp3$x_1[1], x_2 = temp3$x_2[1], pvalue))
    
  }
  
  data_stage2 = rbindlist(data_stage2_1)
  
  ##############################################################################
  # ----------------------------------------------------------------------------
  # -- Define Stochastic Ordering ----------------------------------------------
  pval_final = rbind(data_stage1, data_stage2)
  
  pval_final = pval_final[order(-pvalue)]
  
  pval_final[, number := .GRP, by = pvalue]
  
  pval_final1 = pval_final[, .(x_1, x_2, number)]
  
  ##############################################################################
  # -- Calculate Confidence Interval -------------------------------------------
  
  pi = seq(0, 1, by = 0.0001)
  
  data_CI = foreach(i = 1 : length(pval_final1$number)) %do% {
    
    temp  = pval_final1[i, ]; temp_num = temp$number
    temp1 = pval_final1[number >= temp_num]
    
    temp2 = rbindlist(lapply(pi, function(pi_0){
      
      temp1[, pmf_x1 := dbinom(x_1, n_1, pi_0)]
      temp1[, pmf_x2 := ifelse(is.na(x_2), NA, dbinom(x_2, n_2, pi_0))]
      
      temp1[, pmf := ifelse(is.na(x_2), pmf_x1, pmf_x1*pmf_x2)]
      
      cum_pmf = sum(temp1$pmf)
      
      return(data.table(pi_0, cum_pmf))
    }))
    
    temp3 = temp2[cum_pmf >= alpha]
    
    return(data.table(n_1, n_2, temp, CI_L = min(temp3$pi_0)))
    
  }
  
  data_CI = rbindlist(data_CI)
  
  return(data_CI)
} # Finish CI.ft.Simon


################################################################################
# -- CP function for Shan, Our Design ------------------------------------------
CI.ft.Other = function(data){
  
  n_1 = data$n_1[1]
  r_1 = data$r_1[1]
  n_2 = data$n_2[1]
  
  # ----------------------------------------------------------------------------
  # generate x_2 by x_1 without using foreach
  # for x_1 <= r_1, x_2 is NA
  # for x_1 > r_1, generate a sequence of x_2
  # x_1 <= r_1인 경우 x_2를 NA로 초기화합니다.
  data[x_1 <= r_1, x_2 := NA_integer_]
  
  # x_1 > r_1인 경우 각 조합에 대해 x_2의 시퀀스를 생성합니다.
  data_x2 <- data[x_1 > r_1, .(x_2 = seq(0, n_2, by = 1)), by = .(x_1, n_1, n_2, r_1, r)]
  
  # 데이터를 결합합니다: NA를 포함한 원본 데이터와 새로운 x_2 시퀀스가 있는 데이터
  data <- rbind(data[x_1 <= r_1], data_x2, fill = TRUE)
  
  
  # ----------------------------------------------------------------------------
  # calculate stage1
  data_stage1 = data[is.na(x_2)]
  data_stage1[, pvalue := ifelse(x_1 == 0, 1, pbinom((x_1-1), n_1, pi, lower.tail = F))]
  
  data_stage1 = data_stage1[, .(x_1, x_2, pvalue)]
  
  # ----------------------------------------------------------------------------
  # calculate stage2
  data_stage2 = data[!is.na(x_2)]
  
  data_stage2_1 = foreach(i = 1 : length(data_stage2$x_1)) %do% {
    
    temp3 = data_stage2[i, ]
    
    temp3_1 = temp3 
    
    stage2 = temp3_1[, .(a = seq(temp3$x_1, n_1, by = 1)), by = .(n_1, n_2, r_1, r, x_1, x_2)]
    stage2 = stage2[, x_1_x_2_a := x_1 + x_2 - a]
    
    stage2_final = stage2
    stage2_final[, x_1_x_2_a:= ifelse(x_1_x_2_a <0, 0, x_1_x_2_a)]
    
    stage2_final = stage2_final[, dbin := dbinom(a, n_1, pi)]
    stage2_final = stage2_final[, pbin := pbinom((x_1_x_2_a - 1), n_2, pi, lower.tail = F)]
    stage2_final = stage2_final[, dbin_pbin :=  dbin * pbin]
    
    pvalue = sum(stage2_final$dbin_pbin)
    
    return(data.table(x_1 = temp3$x_1[1], x_2 = temp3$x_2[1], pvalue))
    
  }
  
  data_stage2 = rbindlist(data_stage2_1)
  
  pval_final = rbind(data_stage1, data_stage2)
  
  pval_final = pval_final[order(-pvalue)]
  
  ############################################################################
  # -- Calculate Coverage Probability ----------------------------------------
  pval_final[, number := .GRP, by = pvalue]
  
  pval_final1 = pval_final[, .(x_1, x_2, number)]
  
  pi = seq(0, 1, by = 0.0001)
  
  data_CI = foreach(i = 1 : length(pval_final1$number)) %do% {
    
    temp  = pval_final1[i, ]
    temp1 = pval_final1[number >= i]
    
    temp2 = rbindlist(lapply(pi, function(pi_0){
      
      temp1[, pmf_x1 := dbinom(x_1, n_1, pi_0)]
      temp1[, pmf_x2 := ifelse(is.na(x_2), NA, dbinom(x_2, n_2, pi_0))]
      
      temp1[, pmf := ifelse(is.na(x_2), pmf_x1, pmf_x1*pmf_x2)]
      
      cum_pmf = sum(temp1$pmf)
      
      return(data.table(pi_0, cum_pmf))
    }))
    
    temp3 = temp2[cum_pmf >= alpha]
    
    return(data.table(n_1, n_2, temp, CI_L = min(temp3$pi_0)))
    
  }
  
  data_CI = rbindlist(data_CI)
  
  return(data_CI)
} # Finish CI.ft.Other

##############################################################################
# -- Calculate AL ------------------------------------------------------------

AL.ft = function(data_CI){
  
  AL = data_CI[!is.na(x_2)]
  
  AL[, Length_CI := (1 - CI_L)]
  AL_Value = sum(AL$Length_CI)/length(AL$Length_CI)
  
  return(AL_Value)
}

# -- Calculate AL ------------------------------------------------------------

EL.ft = function(data_CI, pi_0){
  
  EL = data_CI[!is.na(x_2)]
  
  EL[, Length_CI := (1 - CI_L)]
  EL[, bin := dbinom(x_1, n_1, pi_0)*dbinom(x_2, n_2, pi_0)]
  EL[, Length_CI_bin := Length_CI*bin]
  
  EL_Value = sum(EL$Length_CI_bin)
  
  return(EL_Value)
}


################################################################################

# -- Calculate Coverage Porbability ------------------------------------------
CP.ft = function(data_CI){
  
  pi = seq(0, 1, by = 0.0001)
  method1 = rbindlist(lapply(pi, function(pi_0){
    
    temp = data_CI[CI_L <= pi_0]
    
    temp[, pmf_x1 := dbinom(x_1, n_1, pi_0)]
    temp[, pmf_x2 := ifelse(is.na(x_2), NA, dbinom(x_2, n_2, pi_0))]
    
    temp[, pmf := ifelse(is.na(x_2), pmf_x1, pmf_x1*pmf_x2)]
    
    return(data.table(pi_0, coverage.probability = sum(temp$pmf)))
    
  }))
  
  return(method1)
  
}


################################################################################
# -- (Simon, Shan, Our Sample Size data) Calculate CI --------------------------
Calculate.CI = function(data){
  
  pi = data$p_0[1]
  # -- First. Calculate CI in Simon's Design ------
  data_Simon  = data %>% select(contains("Simon"))
  data_Simon[, names(data_Simon) := lapply(.SD, as.numeric)]
  data_Simon1 = data.table(n_1 = data_Simon$Simon_n_1[1],
                           r_1 = (data_Simon$Simon_r_1[1] - 1),
                           n_2 = data_Simon$Simon_n_2[2],
                           r = (data_Simon$Simon_r[2] - 1), 
                           n_2_new = NA)
  
  # CI
  Simon.CI = CI.ft.Simon(data_Simon1)
  Simon.CI = Simon.CI[, Design := "Simon"]
  
  # -- Second Calculate CI in Shan's Design ------
  # -- 1. Data EDA -------------------------------
  pi = data$p_0[1]
  
  data_Shan  = data %>% select(x1, contains("Shan"))
  TEMP       = data_Shan %>% select(x1, Shan_n_2_new)
  TEMP[, names(TEMP) := lapply(.SD, as.numeric)]
  
  names(TEMP) = c("x_1", "n_2_new")
  
  data_Shan0 = data_Shan3 = data_Simon1
  
  data_Shan1 = data_Shan0[, .(x_1 = seq(0, data_Shan$x1[1], by = 1)), by = .(n_1, n_2, r_1, r, n_2_new)]
  
  data_Shan2 = data_Shan0 %>% select(-n_2_new)
  data_Shan2 = data_Shan2[, .(x_1 = seq(data_Shan$x1[2], data_Shan$x1[length(data_Shan$x1) - 1], by = 1)), by = .(n_1, n_2, r_1, r)]
  data_Shan2 = merge(data_Shan2, TEMP, by = c("x_1"), all.x = T)
  
  data_Shan3 = data_Shan3[, .(x_1 = seq(data_Shan$x1[length(data_Shan$x1)], data_Simon1$n_1, by = 1)), by = .(n_1, n_2, r_1, r, n_2_new)]
  
  data_Shan_final = rbind(data_Shan1, data_Shan2, data_Shan3)
  data_Shan_final[, n_2_real := ifelse(is.na(n_2_new), n_2, n_2_new)]
  data_Shan_final = data_Shan_final %>% select(- n_2, -n_2_new)
  names(data_Shan_final)[5] = "n_2"
  
  # -- 2. Calculate C.I. --------------------------
  Shan.CI = CI.ft.Other(data_Shan_final)
  Shan.CI = Shan.CI[, Design := "Shan"]
  
  #################################################
  # -- Third Calculate CI in Our Design -----------
  pi = data$p_0[1]
  
  data_Ours  = data %>% select(x1, contains("Our"))
  TEMP1       = data_Ours %>% select(x1, Ours_n_2_new)
  TEMP1[, names(TEMP1) := lapply(.SD, as.numeric)]
  
  names(TEMP1) = c("x_1", "n_2_new")
  
  data_Ours0 = data_Ours3 = data_Simon1
  
  data_Ours1 = data_Ours0[, .(x_1 = seq(0, data_Ours$x1[1], by = 1)), by = .(n_1, n_2, r_1, r, n_2_new)]
  
  data_Ours2 = data_Ours0 %>% select(-n_2_new)
  data_Ours2 = data_Ours2[, .(x_1 = seq(data_Ours$x1[2], data_Ours$x1[length(data_Ours$x1) - 1], by = 1)), by = .(n_1, n_2, r_1, r)]
  data_Ours2 = merge(data_Ours2, TEMP, by = c("x_1"), all.x = T)
  
  data_Ours3 = data_Ours3[, .(x_1 = seq(data_Ours$x1[length(data_Ours$x1)], data_Simon1$n_1, by = 1)), by = .(n_1, n_2, r_1, r, n_2_new)]
  
  data_Ours_final = rbind(data_Ours1, data_Ours2, data_Ours3)
  data_Ours_final[, n_2_real := ifelse(is.na(n_2_new), n_2, n_2_new)]
  data_Ours_final = data_Ours_final %>% select(- n_2, -n_2_new)
  names(data_Ours_final)[5] = "n_2"
  
  # -- 2. Calculate CI. --------------------------
  Ours.CI = CI.ft.Other(data_Ours_final)
  Ours.CI = Ours.CI[, Design := "Ours"]
  
  #################################################
  Final_data = rbind(Simon.CI, Shan.CI, Ours.CI)
  
  return(Final_data)
  
} # finish Calculate.CI ft



Calculate.CP = function(data){
  
  # CP
  # pi = seq(0, 1, by = 0.0001)
  
  # -- First. Calculate CP in Simon's Design ------
  data_Simon  = data[Design == "Simon"] %>% select(-Design)
  Simon.CP = CP.ft(data_Simon)
  
  Simon.CP = Simon.CP[, Design := "Simon"]
  
  
  # -- Second Calculate CP in Shan's Design ------
  
  data_Shan = data[Design == "Shan"] %>% select(-Design)
  
  Shan.CP =  CP.ft(data_Shan)
  Shan.CP = Shan.CP[, Design := "Shan"]
  
  # -- Third Calculate CP in Our Design -----------
  
  data_Ours = data[Design == "Ours"] %>% select(-Design)
  
  Ours.CP =  CP.ft(data_Ours)
  Ours.CP = Ours.CP[, Design := "Ours"]
  
  Final_data = rbind(Simon.CP, Shan.CP, Ours.CP)
  
  return(Final_data)
  
} # finish Calculate.CP ft