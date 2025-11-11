GetPowerStage1 <- function(standardized_effect_size = 0.30,
                           type1_error_rate = 0.05, 
                           sample_size = 100,
                           prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerStage2WhenEqual <- function(standardized_effect_size = 0.30,
                                    type1_error_rate = 0.05, 
                                    sample_size = 100,
                                    r = 0.40){
  
  q_parameter <- 4/(1-r)
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerStage2 <- function(standardized_effect_size = 0.30,
                           type1_error_rate = 0.05, 
                           sample_size = 100,
                           prob_stage1 = 0.50,
                           response_1 = 0.40,
                           response_0 = 0.40,
                           prob_stage2_given_1 = 0.50,
                           prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - response_1)*prob_stage2_given_1 + (1 - prob_stage1)*(1 - response_0)*prob_stage2_given_0) + 1/(prob_stage1*(1 - response_1)*(1 - prob_stage2_given_1) + (1 - prob_stage1)*(1 - response_0)*(1 - prob_stage2_given_0))
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerSameStartADI <- function(standardized_effect_size = 0.30,
                                 type1_error_rate = 0.05, 
                                 sample_size = 100,
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
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerDifferentStartADI <- function(standardized_effect_size = 0.30,
                                      type1_error_rate = 0.05, 
                                      sample_size = 100,
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
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}


