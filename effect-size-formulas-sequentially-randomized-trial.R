GetEffectSizeStage1 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeStage2 <- function(type1_error_rate = 0.05, 
                                sample_size = 100,
                                power_test = 0.80,
                                prob_stage1 = 0.50,
                                prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/((1 - prob_stage1)*(prob_stage2_given_0)) + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}

GetEffectSizeDifferentStages <- function(type1_error_rate = 0.05, 
                                         sample_size = 100,
                                         power_test = 0.80,
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
  
  effect_size <- (qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test)) * sqrt(q_parameter / sample_size)
  
  return(effect_size)
}
