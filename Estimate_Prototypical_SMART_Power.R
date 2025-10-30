Estimate_Prototypical_SMART_Power <- function( n_sims,
                                               N, 
                                               r_in_first_arm,
                                               r_in_second_arm ,
                                               p1 = 0.5,
                                               p2_in_first_arm = 0.5,
                                               p2_in_second_arm = 0.5,
                                               beta_0 = 1,  # just the intercept, doesn't really matter 
                                               beta_a1 = .5 *sigma / 2, # 'medium' Cohen's d in +1/-1 effect coding
                                               beta_a2 = .5 *sigma / 2, # 'medium' Cohen's d in +1/-1 effect coding
                                               beta_ixn = 0,
                                               corr_eff = .3,
                                               sigma = 1) {
  
  print("========================================================")
  
  all_estimates <- NULL
  all_sigmas <- NULL
  all_pvalues <- NULL
  all_SEs <- NULL
  all_marginal_effect_a1 <- NULL
  all_marginal_effect_a2 <- NULL
  all_marginal_sigma_a1 <- NULL
  all_marginal_sigma_a2 <- NULL 
  all_naive_pvalues_a1 <- NULL
  all_naive_pvalues_a2 <- NULL
  
  # Simulate power
  for (this_sim in 1:n_sims) {
    sim_data <- Simulate_Prototypical_SMART( N = N ,
                                             p1 = p1 ,
                                             r_in_second_arm = r_in_second_arm ,
                                             r_in_first_arm = r_in_first_arm,
                                             p2_in_second_arm = p2_in_second_arm ,
                                             p2_in_first_arm = p2_in_first_arm ,
                                             beta_0 = beta_0 ,
                                             beta_a1 = beta_a1 ,
                                             beta_a2 = beta_a2 ,
                                             beta_ixn = beta_ixn,
                                             sigma = sigma,
                                             corr_eff = corr_eff )
    sim_results <- Analyze_Prototypical_SMART(sim_data,
                                              p1 = p1 ,
                                              p2_in_second_arm = p2_in_second_arm ,
                                              p2_in_first_arm = p2_in_first_arm ) 
    estimates_vector <- c(sim_results$coefficients[,"Estimate"],
                          sim_results$contrasts[,"estimate"])
    names(estimates_vector) <- c(rownames(sim_results$coefficients),rownames(sim_results$contrasts))
    SEs_vector <- c(sim_results$coefficients[,"Std.err"],
                    sim_results$contrasts[,"SE"])
    
    pvalues_vector <- c(sim_results$coefficients[,"Pr(>|W|)"],
                        sim_results$contrasts[,"pvalue"])
    names(pvalues_vector) <- c(rownames(sim_results$coefficients),rownames(sim_results$contrasts))
    all_estimates <- rbind(all_estimates, estimates_vector)
    all_pvalues <- rbind(all_pvalues, pvalues_vector)
    all_SEs <- rbind(all_SEs, SEs_vector)
    all_sigmas <- c(all_sigmas, sim_results$marginal_sd_estimate)
    all_marginal_effect_a1 <- rbind(all_marginal_effect_a1, 2*sim_results$naive_way_effect1["Estimate"])
    all_marginal_effect_a2 <- rbind(all_marginal_effect_a2, 2*sim_results$naive_way_effect2["Estimate"])
    all_marginal_sigma_a1 <- rbind(all_marginal_sigma_a1, sim_results$naive_way_sd1)
    all_marginal_sigma_a2 <- rbind(all_marginal_sigma_a2, sim_results$naive_way_sd2)
    all_naive_pvalues_a1 <- rbind(all_naive_pvalues_a1, sim_results$naive_way_effect1["Pr(>|t|)"])
    all_naive_pvalues_a2 <- rbind(all_naive_pvalues_a2, sim_results$naive_way_effect2["Pr(>|t|)"])
  }
  
  
  # Get ready to calculate performance measures, by first calculating true values
  true_coefs <- c(beta_0,
                  beta_a1,
                  beta_a2,
                  beta_ixn)  
  true_contrasts <- sim_results$contrast_coefficients %*% true_coefs
  
  
  # Calculate bias 
  print("Observed power:")
  simulated_power <- apply(all_pvalues < .05,2,mean)
  print(simulated_power)
  
  
  
  
  ############################################################ 
  
  # estimates of raw effect (capital Delta) variance
  
  q_a1 <- 1/(p1*(1-p1))
  predicted_var_Delta <- (sigma^2)*q_a1/N
  observed_var_Delta <- var(2*all_estimates[,"A1"])  # the multiplier two is to turn an effect-coded regression coefficient (+1,-1) into a mean difference (because +1-(-1)=2)
  
  
  # calculated power
  ############################################################
  print("Power for Main Effect of A1")
  
  delta_a1 <- 2*beta_a1/sigma # lower case delta
  formula_power_a1 <- pnorm(delta_a1 * sqrt(N/q_a1) - qnorm(.975))
  simulated_power_a1 <- as.numeric(simulated_power["A1"])
  simulated_power_a1_simple <- mean(all_naive_pvalues_a1 < .05)
  print(c(formula_power_a1=formula_power_a1, 
          simulated_power_a1=simulated_power_a1))
  
  ############################################################
  print("Power for Main Effect of A2")
  q_a2 <- (p1*(1-r_in_first_arm)*p2_in_first_arm + (1-p1)*(1-r_in_second_arm)*p2_in_second_arm)^(-1) +
    (p1*(1-r_in_first_arm)*(1-p2_in_first_arm) + (1-p1)*(1-r_in_second_arm)*(1-p2_in_second_arm))^(-1)
  delta_a2 <- 2*beta_a2/sigma # lower case delta
  formula_power_a2 <- pnorm(delta_a2 * sqrt(N/q_a2) - qnorm(.975))
  simulated_power_a2 <- as.numeric(simulated_power["A2"])
  print(c(formula_power_a2=formula_power_a2, 
          simulated_power_a2=simulated_power_a2))
  
  ############################################################
  print("Power for Main Effect of A2, simplified")
  q_a2_simple <- 4/(1-((r_in_first_arm+r_in_second_arm)/2))
  delta_a2_simple <- 2*beta_a2/sigma # lower case delta
  formula_power_a2_simple <- pnorm(delta_a2 * sqrt(N/q_a2) - qnorm(.975))
  simulated_power_a2_simple <- mean(all_naive_pvalues_a2 < .05)
  
  ############################################################
  print("Power for ++ versus -+")
  
  q_pp_vs_mp <- r_in_first_arm / p1 +
    (1-r_in_first_arm)/(p1*p2_in_first_arm) + 
    r_in_second_arm / (1-p1) + 
    (1-r_in_second_arm)/((1-p1)*p2_in_second_arm)
  observed_mean_Delta <- mean(all_estimates[,"contrast of ++ versus -+"])
  predicted_var_Delta <- (sigma^2)*q_pp_vs_mp/N
  observed_var_Delta <- var(all_estimates[,"contrast of ++ versus -+"])
  
  delta_pp_vs_mp <- 2*(beta_a1+beta_ixn)/sigma # lower case delta
  formula_power_pp_vs_mp <- pnorm(delta_pp_vs_mp * sqrt(N/q_pp_vs_mp) - qnorm(.975))
  simulated_power_pp_vs_mp <- as.numeric(simulated_power["contrast of ++ versus -+"])
  print(c(formula_power_pp_vs_mp=formula_power_pp_vs_mp, 
          simulated_power_pp_vs_mp=simulated_power_pp_vs_mp))
  
  ############################################################
  print("Power for ++ versus +-")
  
  q_pp_vs_pm <- (1/(p1*(1-r_in_first_arm))) * 
    ((1/p2_in_first_arm)+(1/(1-p2_in_first_arm)))  
  
  observed_mean_Delta <- mean(all_estimates[,"contrast of ++ versus +-"])
  predicted_var_Delta <- (sigma^2)*q_pp_vs_pm/N
  observed_var_Delta <- var(all_estimates[,"contrast of ++ versus +-"])
  
  delta_pp_vs_pm <- 2*(beta_a2+beta_ixn)/sigma # lower case delta
  formula_power_pp_vs_pm <- pnorm(delta_pp_vs_pm * sqrt(N/q_pp_vs_pm) - qnorm(.975))
  simulated_power_pp_vs_pm <- as.numeric(simulated_power["contrast of ++ versus +-"])
  print(c(formula_power_pp_vs_pm=formula_power_pp_vs_pm, 
          simulated_power_pp_vs_pm=simulated_power_pp_vs_pm))
  
  
  ############################################################
  print("Power for ++ versus --")
  
  q_pp_vs_mm <- r_in_first_arm / p1 +
    (1-r_in_first_arm)/(p1*p2_in_first_arm) + 
    r_in_second_arm / (1-p1) + 
    (1-r_in_second_arm)/((1-p1)*(1-p2_in_second_arm))
  observed_mean_Delta <- mean(all_estimates[,"contrast of ++ versus --"])
  predicted_var_Delta <- (sigma^2)*q_pp_vs_mm/N
  observed_var_Delta <- var(all_estimates[,"contrast of ++ versus --"])
  
  delta_pp_vs_mm <- 2*(beta_a1+beta_ixn)/sigma # lower case delta
  formula_power_pp_vs_mm <- pnorm(delta_pp_vs_mm * sqrt(N/q_pp_vs_mm) - qnorm(.975))
  simulated_power_pp_vs_mm <- as.numeric(simulated_power["contrast of ++ versus --"])
  print(c(formula_power_pp_vs_mm=formula_power_pp_vs_mm, 
          simulated_power_pp_vs_mm=simulated_power_pp_vs_mm))
  
  ##############################################################
  
  options(scipen=old_scipen)
  
  return(list(    marginal_effect_a1=mean(all_marginal_effect_a1),
                  marginal_effect_a2=mean(all_marginal_effect_a2),
                  marginal_sigma_a1=mean(all_marginal_sigma_a1),
                  marginal_sigma_a2=mean(all_marginal_sigma_a2),
                  formula_power_a1=formula_power_a1, 
                  simulated_power_a1=simulated_power_a1,
                  simulated_power_a1_simple=simulated_power_a1_simple,
                  formula_power_a2=formula_power_a2, 
                  simulated_power_a2=simulated_power_a2,
                  formula_power_a2_simple=formula_power_a2_simple, 
                  simulated_power_a2_simple=simulated_power_a2_simple,
                  formula_power_pp_vs_mp=formula_power_pp_vs_mp, 
                  simulated_power_pp_vs_mp=simulated_power_pp_vs_mp,
                  formula_power_pp_vs_pm=formula_power_pp_vs_pm, 
                  simulated_power_pp_vs_pm=simulated_power_pp_vs_pm,
                  formula_power_pp_vs_mm=formula_power_pp_vs_mm, 
                  simulated_power_pp_vs_mm=simulated_power_pp_vs_mm
  ))
}
#############################################
