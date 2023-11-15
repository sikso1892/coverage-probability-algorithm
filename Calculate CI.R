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
  Simon.CI[, Design := "Simon"]
  
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
  
  pi = data$p_0[1]
  Shan.CI = CI.ft.Other(data_Shan_final)
  Shan.CI[, Design := "Shan"]
  
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
  Ours.CI[, Design := "Ours"]
  
  #################################################
  Final_data = rbind(Simon.CI, Shan.CI, Ours.CI)
  
  return(Final_data)
  
} # finish Calculate.CI ft