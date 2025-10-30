#####################################################
# Code to simulate a prototypical sequential multiple
# assignment randomized trial (SMART).
# By John Dziak, d3Center, University of Michigan
# See:
# Nahum-Shani I, Qian M, Almirall D, Pelham WE, Gnagy B, 
# Fabiano GA, Waxmonsky JG, Yu J, Murphy SA. Experimental 
# design and primary data analysis methods for comparing 
# adaptive interventions. Psychological methods. 2012 Dec;
# 17(4):457.
Analyze_Prototypical_SMART <- function(this_data,
                                       p1 = 0.5,
                                       p2_in_first_arm = 0.5,
                                       p2_in_second_arm = 0.5) {
  # The object called this_data is assumed to be in the same format
  # as the output from Simulate_Prototypical_SMART, with the same
  # variable names.
  require(dplyr)  # Will use the dplyr package
  require(geepack)  # Will use the geepack package
  
  this_data <- this_data %>% mutate(
    prob_of_received_A1 = case_match(A1,
                                     +1 ~ p1,  # If you were assigned A1=+1, then the unconditional probability of 
                                     # getting what you did in fact get for A1 was P(A1=+1) which is p1.
                                     -1 ~ 1-p1 # If you were assigned A1=-1, then the unconditional probability of 
                                     # getting what you did in fact get for A1 was P(A1=-1) which is 1-p1.
    ), 
    stage1_weight = 1/prob_of_received_A1,
    prob_of_plus_one_A2 = case_match(A1,
                                     +1 ~ p2_in_first_arm,  # P(A2=+1 | A1 = +1)
                                     -1 ~ p2_in_second_arm ),  # P(A2=+1 | A1 = -1)
    prob_of_received_A2 = case_match(A2,
                                     +1 ~ prob_of_plus_one_A2,  # P(A2 | A1) for A1=whatever it was for that participant, and A2=+1
                                     -1 ~ 1-prob_of_plus_one_A2), # P(A2 | A1) for A1=whatever it was for that participant, and A2=-1
    stage2_weight = case_match(R, 
                               0 ~ 1/prob_of_received_A2,
                               1 ~ 1),
    design_weight = stage1_weight*stage2_weight)
  # Assign known weights (explained in, e.g., Nahum-Shani et al., 2012).
  # Nonresponders (R=0) get a weight of 2, and responders (R=1) get a weight of 4.
  # This is similar to inverse propensity weighting.
  
  rows_to_replicate <- this_data %>% filter(R==1)
  rows_not_to_replicate <- this_data %>% filter(R==0) %>% mutate(replicant=0)
  # Only responders are replicated in the weighting and replication
  # approach for the prototypical SMART.
  # For each responder, a pseudo-replicate is created for each 
  # possible value of A2 they could have been given, i.e., -1 and +1.
  positive_pseudo_replicates <- rows_to_replicate %>% mutate(A2=+1, replicant=1)
  negative_pseudo_replicates <- rows_to_replicate %>% mutate(A2=-1, replicant=2)
  weighted_replicated_data <- rbind(positive_pseudo_replicates,
                                    negative_pseudo_replicates,
                                    rows_not_to_replicate) %>%
    arrange(id)  # wE have to sort by id to show that the pseudo-replicates are clustered, 
  # and not separate "real" people; otherwise the sample size will be miscounted
  # by geeglm.
  # Now we can solve the weighted estimating equations using working 
  # independence GEE in, for example, the geeglm function in the geepack package.
  gee1 <- geeglm(formula = Y ~  A1 + A2 + A1:A2,  
                 id = id,
                 weights = design_weight,    
                 data = weighted_replicated_data)
  
  coefficients_table <-  summary(gee1)$coefficients 
  # This object contains the estimation and test output, to be returned 
  # in case the user wants to look at it.
  
  contrast_coefficients <-   matrix(c( 
    1, +1, +1, +1,  #  mean of ++
    1, +1, -1, -1,	#  mean of +-
    1, -1, +1, -1,	#  mean of -+
    1, -1, -1, +1,	#  mean of --
    0,  2,  0,  0,  # contrast of +. versus -.
    0,  0,  2,  0,  # contrast of .+ versus .-
    0,  0,  2,  2,  #  contrast of ++ versus +-
    0,  2,  0,  2,  #  contrast of ++ versus -+
    0,  2,  2,  0,  #  contrast of ++ versus --
    0,  2, -2,  0,	#  contrast of +- versus -+
    0,  2,  0, -2,	#  contrast of +- versus --
    0,  0,  2, -2), #  contrast of -+ versus --
    byrow=TRUE,ncol=4)
  # Cross-multiplying each of these vectors by the vector of
  # estimated regression coefficients will give estimated linear 
  # contrasts.
  rownames(contrast_coefficients) <- c( "mean of ++ versus zero",
                                        "mean of +- versus zero",
                                        "mean of -+ versus zero",
                                        "mean of -- versus zero",
                                        "contrast of +. versus -.",
                                        "contrast of .+ versus .-",
                                        "contrast of ++ versus +-",
                                        "contrast of ++ versus -+",
                                        "contrast of ++ versus --",
                                        "contrast of +- versus -+",
                                        "contrast of +- versus --",
                                        "contrast of -+ versus --"
  )
  # Compute estimated linear contrasts:
  linear_contrast_estimates <- contrast_coefficients %*% coef(gee1)
  # Compute their standard errors using the multivariate delta method:
  linear_contrast_SEs <- sqrt(diag(contrast_coefficients %*% (gee1$geese$vbeta) %*% t(contrast_coefficients)))
  # Compute their p-values for testing whether the expected contrast equals zero
  linear_contrast_pvalues <- 2 * (1-pnorm( abs(linear_contrast_estimates/linear_contrast_SEs)))
  # Combine linear contrast information into a data frame for the output
  linear_contrast_tests <- data.frame(estimate=linear_contrast_estimates,
                                      SE=linear_contrast_SEs,
                                      pvalue=linear_contrast_pvalues)
  
  effect1_model_simple <- lm(Y~A1, data=this_data)
  effect2_model_simple <- lm(Y~A2, data=this_data %>% filter(R==0))
  
  
  # Return the results:
  return(list(coefficients=coefficients_table,
              contrast_coefficients=contrast_coefficients,
              contrasts=linear_contrast_tests,
              marginal_sd_estimate=summary(gee1)$dispersion$Estimate,
              weighted_replicated_data=weighted_replicated_data,
              naive_way_effect1=summary(effect1_model_simple)$coefficients["A1",],
              naive_way_sd1=sd(effect1_model_simple$residuals),
              naive_way_effect2=summary(effect2_model_simple)$coefficients["A2",],
              naive_way_sd2=sd(effect2_model_simple$residuals),
              gee_results=gee1))
}