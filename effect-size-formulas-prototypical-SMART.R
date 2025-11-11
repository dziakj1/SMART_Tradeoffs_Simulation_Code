GetEffectSizeStage1 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeStage2WhenEqual <- function(type1_error_rate = 0.05, 
                                         sample_size = 100,
                                         power_test = 0.80,
                                         r = 0.40){
  
  q_parameter <- 4/(1-r)
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeStage2 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50,
                                response_1 = 0.40,
                                response_0 = 0.40,
                                prob_stage2_given_1 = 0.50,
                                prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - response_1)*prob_stage2_given_1 + (1 - prob_stage1)*(1 - response_0)*prob_stage2_given_0) + 1/(prob_stage1*(1 - response_1)*(1 - prob_stage2_given_1) + (1 - prob_stage1)*(1 - response_0)*(1 - prob_stage2_given_0))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeSameStartADI <- function(type1_error_rate = 0.05, 
                                      sample_size = 100,
                                      power_test = 0.80,
                                      prob_stage1 = 0.50,
                                      response_1 = 0.40,
                                      response_0 = 0.40,
                                      prob_stage2_given_1 = 0.50,
                                      prob_stage2_given_0 = 0.50,
                                      a1 = 1){
  
  if(a1 == 1){
    p_a1 <- prob_stage1
    r_a1 <- response_1
    prob_stage2_given_a1 <- prob_stage2_given_1
  }else{
    p_a1 <- 1 - prob_stage1
    r_a1 <- response_0
    prob_stage2_given_a1 <- prob_stage2_given_0
  }
  
  q_parameter <- (1/(p_a1 * (1 - r_a1)))*((1/prob_stage2_given_a1) + (1/(1 - prob_stage2_given_a1)))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeDifferentStartADI <- function(type1_error_rate = 0.05, 
                                           sample_size = 100,
                                           power_test = 0.80,
                                           prob_stage1 = 0.50,
                                           response_1 = 0.40,
                                           response_0 = 0.40,
                                           prob_stage2_given_1 = 0.50,
                                           prob_stage2_given_0 = 0.50,
                                           a2_given_1 = 1,
                                           b2_given_0 = 1){
  
  if(a2_given_1 == 1){
    p_a2_given_1 <- prob_stage2_given_1
  }else{
    p_a2_given_1 <- 1 - prob_stage2_given_1
  }
  
  if(b2_given_0 == 1){
    p_b2_given_0 <- prob_stage2_given_0
  }else{
    p_b2_given_0 <- 1 - prob_stage2_given_0
  }
  
  q_parameter <- (response_1/prob_stage1) + (response_0/(1 - prob_stage1)) + ((1 - response_1)/(prob_stage1 * p_a2_given_1)) + ((1 - response_0)/((1 - prob_stage1) * p_b2_given_0))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}
