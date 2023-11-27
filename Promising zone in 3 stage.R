# -- Adaptive design in 3 stage design -----------------------------------------
library(foreach)
library(data.table)
library(dplyr)

# -- 1. Find_d_ft --------------------------------------------------------------

Find_d_ft = function(data){
  
  n_1 = data$n_1; n_2 = data$n_2; n_3 = data$n_3
  r_1 = data$r_1; r_2 = data$r_2; r_3 = data$r_3
  p_1 = data$p1
  power = 1 - data$beta
  
  # 벡터화된 pow 계산
  j_values = r_2:(n_1 + n_2)
  
  find_d = list()
  for(j in j_values){
    
    pow_values = pbinom(q = (r_3 - j - 1), size = n_3, prob = p_1, lower.tail = FALSE)
    find_d[[j]] = data.table(n_1, n_2, r_1, r_2, j = j, pow_values)
    
  }
  find_d = rbindlist(find_d) 
  
  d = dim(find_d[pow_values < power])[1]
  
  return(d)
}

# -- 2. Find_b_ft --------------------------------------------------------------

Find_b_ft = function(data, CP_0_value){
  
  n_1 = data$n_1; n_2 = data$n_2; n_3 = data$n_3
  r_1 = data$r_1; r_2 = data$r_2; r_3 = data$r_3
  p_1 = data$p1
  
  CP_0 = CP_0_value
  
  # 벡터화된 pow 계산
  j_values = r_2:(n_1 + n_2)
  
  find_b = foreach(j = j_values) %do% {
    
    #  power >= 1 - beta 만족하면 stop 
    pow_values = pbinom(q = (r_3 - j - 1), size = n_3, prob = p_1, lower.tail = F)
    
    return(data.table(n_1, n_2, r_1, r_2, j = j, pow_values))
    
  }
  
  find_b = rbindlist(find_b) 
  
  b = dim(find_b[pow_values < CP_0])[1]
  
  return(b)
}


# -- 3. PST1 function ----------------------------------------------------------
PST1 = function(n_1, r_1, pi){
  
  pst1 = pbinom((r_1 - 1), n_1, pi, lower.tail = T)
  return(pst1) 
}
# -- 5. PST2 function ----------------------------------------------------------
PST2 = function(n_1, r_1, n_2, r_2, pi){
  
  pst2 = 0
  for(x_1 in r_1 : min(n_1, (r_2-1))){
    
    pst2 = pst2 + dbinom(x_1, n_1, pi)*pbinom((r_2-1 - x_1), n_2, pi, lower.tail = T)
    
  }
  
  return(pst2) 
}
# -- 6. PST3 function ----------------------------------------------------------
PST3 = function(n_1, r_1, n_2, r_2, n_3, r_3, pi){
  
  pst3 = 0
  for(x_1 in r_1 : min(n_1, (r_3-1))){
    
    for(x_2 in (r_2 - x_1) : min(n_2, (r_3 - x_1-1))){
    
      pst3 = pst3 + dbinom(x_1, n_1, pi)*dbinom(x_2, n_2, pi)*pbinom((r_3 - x_1 - x_2-1), n_3, pi, lower.tail = T)
    }
  }
  
  return(pst3) 
}
# -- 7. Simon_alpha_Promising function -----------------------------------------

Chen_alpha_Promising = function(data, CP_0_value){
  
  n_1 = data$n_1; n_2 = data$n_2; n_3 = data$n_3
  r_1 = data$r_1; r_2 = data$r_2; r_3 = data$r_3
  p_0 = data$p0
  p_1 = data$p1
  
  b = Find_b_ft(data, CP_0_value)
  
  P1 = PST1(n_1, r_1, p_0)
  P2 = PST2(n_1, r_1, n_2, r_2, p_0)
  P3 = PST3(n_1, r_1, n_2, (r_2+b), n_3, r_3, p_0)
  
  alpha = 1 - (P1 + P2 + P3)
  return(alpha)
}

################################################################################
# -- Find_SS_ft ----------------------------------------------------------------
Find_SS_Promising_ft = function(data, CP_0_value){
  
  n_1 = data$n_1; n_2 = data$n_2; n_3 = data$n_3
  r_1 = data$r_1; r_2 = data$r_2; r_3 = data$r_3
  p_0 = data$p0
  p_1 = data$p1
  alpha = data$alpha
  beta = data$beta
  power= 1 - beta
  
  N = n_1 + n_2 + n_3
  
  d = Find_d_ft(data)
  b = Find_b_ft(data, CP_0_value)
  
  alpha_chen = Chen_alpha_Promising(data, CP_0_value)
  
  
  if (d == 0) {
    # d가 0이면, 원래의 input data를 반환합니다.
    return(data.table(data, Comment = "Original"))
  } else {
  
  # Final data
  data_final = foreach(n_3_new = n_3 : (n_1 + n_2 + N)) %do% {
    
    N_new = n_1 + n_2 + n_3_new
    
    # set r_prime
    temp = data.table(n_1, n_2, n_3,
                      r_1, r_2, r_3,
                      n_3_new,
                      r_3_new = seq(r_3, N_new, by = 1))
    
    temp1 <- lapply(seq_len(nrow(temp)), function(i) as.list(temp[i,]))
    
    temp2 = lapply(temp1, function(x){
      
      r_3_new   = x$r_3_new
      
      # set xi2
      xi2 = (alpha - alpha_chen)/(d - b)
      
      pow_new = alp_new = origin_alp = 0
      aa = foreach(j = (r_2 + b) : (r_2 + d - 1)) %do% {
        
        # power = P[H0 기각 | H1]
        pow_new    = pbinom(q = (r_3_new - j - 1), size = n_3_new, prob = p_1, lower.tail = F)  
        
        # alpha : Use Mid-Pvalue
        alp_new    = 0.5*dbinom((r_3_new - j ), n_3_new, prob = p_0) + pbinom(q = (r_3_new - j ), size = n_3_new, prob = p_0, lower.tail = F)  
        
        origin_alp = pbinom(q = (r_3 - j - 1),       size = n_3,     prob = p_0, lower.tail = F) 
        
        alp2 = origin_alp + xi2 
        # a2 = a + eta1
        
        return(data.table(p_0, p_1, alpha, beta, n_1, n_2, n_3, r_1, r_2, r_3, r_3_new, n_3_new, N_new, j, pow_new, alp_new, origin_alp, xi2, alp2))
        
        pow_new = alp_new = origin_alp = 0
      }
      aa = rbindlist(aa)
      
      return(aa)
    }) # temp2 
    
    temp3 = rbindlist(temp2)
    
    # equation (4) (alpha) 사용
    temp4 = temp3[alp_new <= alp2]
    
    # equation beta 사용
    tau = 0
    temp5 = temp4[pow_new  >= (power + tau)]
    
    temp7 = temp5
    
    if (length(temp5) == 0) {
      # pow_new < 1 - beta 인 경우
      temp5_1 = temp4[pow_new < power]
      
      tau = 0.005
      temp6 = temp5_1[pow_new >= (power + tau)]
      temp7 = rbind(temp5, temp6)
    }
    
    return(temp7)
    
  } # for "i" loop(foreach)
  data_final = rbindlist(data_final)

  # j 별 n_3' & r_3' 찾기
  j_count = data_final$j %>% unique
  j_count = j_count[order(j_count)]
  
  data_final_1 = foreach(kk = 1 : max(1, (d-b))) %do% {
    
    dat = data_final[j == j_count[kk]]
    
    return(dat[n_3_new == min(n_3_new) & r_3_new == min(r_3_new)])
    
  }
  
  data_final_1 = rbindlist(data_final_1)
  return(data.table(data_final_1, Comment = "Modified"))
  }
  
}