GetPowerStage1 <- function(standardized_effect_size = 0.30,
                           type1_error_rate = 0.05, 
                           sample_size = 100,
                           prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerStage2 <- function(standardized_effect_size = 0.30,
                           type1_error_rate = 0.05, 
                           sample_size = 100,
                           prob_stage1 = 0.50,
                           prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/((1 - prob_stage1)*(prob_stage2_given_0)) + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}

GetPowerDifferentStages <- function(standardized_effect_size = 0.30,
                                    type1_error_rate = 0.05, 
                                    sample_size = 100,
                                    prob_stage1 = 0.50,
                                    prob_stage2_given_0 = 0.50,
                                    a2 = 0){
  
  if(a2 == 0){
    # Cell C
    q_parameter <- 1/prob_stage1 + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  }else{
    # Cell B
    q_parameter <- 1/prob_stage1 + 1/((1 - prob_stage1)*(prob_stage2_given_0))
  }
  
  power_test <- pnorm(q = standardized_effect_size*sqrt(sample_size/q_parameter) - qnorm(p = 1 - type1_error_rate/2))
  
  return(power_test)
}


