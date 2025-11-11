GetSampleSizeStage1 <- function(standardized_effect_size = 0.30,
                                type1_error_rate = 0.05, 
                                power_test = 0.80,
                                prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))^2) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeStage2WhenEqual <- function(standardized_effect_size = 0.30,
                                         type1_error_rate = 0.05, 
                                         power_test = 0.80,
                                         r = 0.40){
  
  q_parameter <- 4/(1-r)
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeStage2 <- function(standardized_effect_size = 0.30,
                                type1_error_rate = 0.05, 
                                power_test = 0.80,
                                prob_stage1 = 0.50,
                                response_1 = 0.40,
                                response_0 = 0.40,
                                prob_stage2_given_1 = 0.50,
                                prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - response_1)*prob_stage2_given_1 + (1 - prob_stage1)*(1 - response_0)*prob_stage2_given_0) + 1/(prob_stage1*(1 - response_1)*(1 - prob_stage2_given_1) + (1 - prob_stage1)*(1 - response_0)*(1 - prob_stage2_given_0))
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeSameStartADI <- function(standardized_effect_size = 0.30,
                                      type1_error_rate = 0.05, 
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
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeDifferentStartADI <- function(standardized_effect_size = 0.30,
                                           type1_error_rate = 0.05, 
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
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}
