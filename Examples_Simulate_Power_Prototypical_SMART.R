rm(list = ls())
set.seed(16802)
library(dplyr)
library(geepack)

old_scipen <- getOption("scipen") # save old options
options(scipen=12)  # change options to temporarily turn off scientific notation
source("Estimate_Prototypical_SMART_Power.R")
source("Simulate_Prototypical_SMART.R")
source("Analyze_Prototypical_SMART.R")


# Example with equal response rates
estimate1 <- Estimate_Prototypical_SMART_Power( n_sims = 1000,
                                                N = 300, 
                                                p1 = 0.5,
                                                r_in_first_arm = 0.6,
                                                r_in_second_arm = 0.6,
                                                p2_in_first_arm = 0.5,
                                                p2_in_second_arm = 0.5,
                                                beta_0 = 1,  # just the intercept 
                                                beta_a1 = .2  / 2, # 'small' Cohen's d in +1/-1 effect coding, given that sigma=1
                                                beta_a2 = .5 / 2, # 'medium' Cohen's d in +1/-1 effect coding, given that sigma=1
                                                beta_ixn = 0,
                                                corr_eff = .3,
                                                sigma = 1)  

# Unequal response rates
estimate2 <-  Estimate_Prototypical_SMART_Power( n_sims = 1000,
                                                 N = 300, 
                                                 p1 = 0.5,
                                                 r_in_second_arm = 0.4,
                                                 r_in_first_arm = 0.8,
                                                 p2_in_first_arm = 0.5,
                                                 p2_in_second_arm = 0.5,
                                                 beta_0 = 1, # just the intercept 
                                                 beta_a1 = .2 / 2, # 'small' Cohen's d in +1/-1 effect coding, given that sigma=1
                                                 beta_a2 = .5 / 2, # 'medium' Cohen's d in +1/-1 effect coding, given that sigma=1
                                                 beta_ixn = 0,
                                                 xi_0 = .3,
                                                 sigma = 1 )  # unbalanced R's 

 
save.image(file="ran-simulations.rdata")