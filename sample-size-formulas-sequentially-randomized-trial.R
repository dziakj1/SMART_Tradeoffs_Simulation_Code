GetSampleSizeStage1 <- function(standardized_effect_size = 0.30,
                                type1_error_rate = 0.05, 
                                power_test = 0.80,
                                prob_stage1 = 0.50){
  
  q_parameter <- 1/(prob_stage1*(1 - prob_stage1))
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))^2) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeStage2 <- function(standardized_effect_size = 0.30,
                                type1_error_rate = 0.05, 
                                power_test = 0.80,
                                prob_stage1 = 0.50,
                                prob_stage2_given_0 = 0.50){
  
  q_parameter <- 1/((1 - prob_stage1)*(prob_stage2_given_0)) + 1/((1 - prob_stage1)*(1 - prob_stage2_given_0))
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))^2) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}

GetSampleSizeDifferentStages <- function(standardized_effect_size = 0.30,
                                         type1_error_rate = 0.05, 
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
  
  sample_size <- ((qnorm(p = 1 - type1_error_rate/2) + qnorm(p = power_test))^2) * q_parameter / (standardized_effect_size^2)
  
  return(sample_size)
}


