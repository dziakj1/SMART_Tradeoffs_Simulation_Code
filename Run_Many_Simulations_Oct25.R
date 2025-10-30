rm(list = ls())
set.seed(18518)
library(dplyr)
library(geepack)

old_scipen <- getOption("scipen") # save old options
options(scipen=12)  # change options to temporarily turn off scientific notation
source("Estimate_Prototypical_SMART_Power.R")
source("Simulate_Prototypical_SMART.R")
source("Analyze_Prototypical_SMART.R")

answers <- NULL
 

scenarios <- expand.grid(
  N = c(200, 400, 600, 800, 1000),
  p1 = c( (1/3), (1/2), (2/3) ),  # P(A1=.25) means an allocation ratio of 1:3 at stage 1
  r1 = c(.35,.65),
  r2 = c(.35,.65),
  effect_size_1 = .3,
  effect_size_2 = .5,
  corr_eff = c(0,.5),
  beta_ixn = c(0,.1)
) 

n_sims <- 1000
start_time <- Sys.time()

for (scenario_index in 1:nrow(scenarios)) {
  
  cat("Starting scenario ")
  cat(scenario_index)
  cat("\n")
  
  this_scenario <- scenarios[scenario_index, ]
  
  estimate1 <- Estimate_Prototypical_SMART_Power( n_sims = n_sims,
                                                  N = this_scenario$N, 
                                                  p1 = this_scenario$p1,
                                                  r_in_first_arm = this_scenario$r1,
                                                  r_in_second_arm = this_scenario$r2,
                                                  p2_in_first_arm = 0.5,
                                                  p2_in_second_arm = 0.5,
                                                  beta_0 = 1,  # the intercept 
                                                  beta_a1 = this_scenario$effect_size_1 / 2,   
                                                  beta_a2 = this_scenario$effect_size_2 / 2, 
                                                  beta_ixn = this_scenario$ beta_ixn, # no interaction
                                                  corr_eff = this_scenario$corr_eff,   # assume 0 so that conditional and marginal parameters always have same meaning
                                                  sigma = 1)  
  
  answers <- rbind(answers, unlist(estimate1))
  
}

answers_data <- data.frame(answers)
print(answers_data)
 

finish_time <- Sys.time()
print(difftime(finish_time, start_time))

save.image(paste(file="ran-many-simulations-",Sys.Date(),".rdata",sep=""))