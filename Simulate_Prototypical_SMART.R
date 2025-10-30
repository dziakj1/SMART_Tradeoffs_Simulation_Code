#####################################################
# Code to simulate a prototypical sequential multiple
# assignment randomized trial (SMART).
# By John Dziak, d3Center, University of Michigan
#####################################################
# Simulates data from a prototypical SMART experiment, with 
# homoskedastic normally distributed outcomes at final assessment.
# The current version of the code is written to support  
# the manuscript:
# Yap, J. R. T., Nahum-Shani, I., & Dziak, J. J. (2025).
# Tradeoffs in Design and Sample Size Planning for 
# Sequential Randomized Trials.
# This work was supported by NIH awards P50 DA 054039 and NIH 
# R01 DA 039901 from the National Institute on Drug Abuse.
# For information on SMART's and prototypical SMART's, see:
#   Murphy SA: An experimental design for the development of 
#     adaptive treatment strategies. Statistics in medicine.
#     2005, 24:1455-1481.
#   Nahum-Shani I, Qian M, Almirall D, et al.: Experimental 
#     design and primary data analysis methods for comparing 
#     adaptive interventions. Psychological Methods. 2012, 17:457.
#   Seewald NJ, Hackworth O, Almirall D: Sequential, multiple 
#     assignment, randomized trials (SMART). Principles and
#     Practice of Clinical Trials: Springer, 2022, 1543-1561.
# The phrase "prototypical SMART" is more clearly defined in the 
# literature review in "Comparing cluster-level dynamic treatment 
# regimens using sequential, multiple assignment, randomized 
# trials: Regression estimation and sample size considerations" (
#   NeCamp, Kilbourne, and Almirall, Statistical Methods in
#   Medical Research, 2017).  It means a SMART with two stages 
#   and a nonresponse assessment between the stages, with all
#   nonresponders re-randomized and no responders randomized. 
#   Here we also take it to assume that each randomization is
#   dichotomous.  The second-stage randomization options could 
#   be different based on first-stage arms, although they should
#   be at least related if the second-stage main effect is to
#   have a clear interpretation. 
#   Note:  We use effect-coding (+1 and -1) here because this makes 
#   coefficients more interpretable in models with interactions, 
#   rather than non-centered dummy coding (1 and 0), when representing
#   levels of factors.  In the derivations within the paper, we 
#   used dummy-coding for notational convenience and compatibility
#   with some past sample size literature.  The most important
#   difference for users of this function is that beta_a1
#   and beta_a2 should be twice the desired contrast between for 
#   comparing +1 to -1.
Simulate_Prototypical_SMART <- function(
    N = 300,  # Total sample size
    r_in_first_arm, # probability that R=1 (responder) in the first arm
    # of the first-stage randomization
    r_in_second_arm, # probability that R=1 (responder) in the first arm
    # of the first-stage randomization
    p1 = .5,   # Probability for first arm in first-stage randomization
    p2_in_first_arm = .5,   # Probability for first arm in second-stage
               # randomization, given that the FIRST arm was selected 
               # in first stage randomization
    p2_in_second_arm = .5,  # Probability for first arm in
               # second-stage randomization, given that the SECOND 
               # arm was selected in first stage randomization
    beta_0,    # intercept in marginal effect-coded model
    beta_a1,   # coefficient for first-stage effect in marginal 
               # effect-coded model.  This is twice the Cohen's d for
               # the first-stage component, if sigma=1.
    beta_a2,   # coefficient for second-stage effect in marginal 
               # effect-coded model.  This is twice the Cohen's d for
               # the second-stage component among nonresponders,
               # if sigma=1.
    beta_ixn,  #  coefficient for interaction in marginal effect-coded model.
    corr_eff, # correlational effect of R, not included in the causal model. 
    sigma = 1,  # error (residual) standard deviation
    decimal_places = 2  # decimal places for simulated normal data
) {
  # Define codes for the first and second levels.
  level_codes = c(+1,-1) # Uses effect coding, not dummy coding, for A1 and A2 
  
  A1 <- sample(level_codes,
               size=N,
               replace=TRUE,
               prob=c(p1,1-p1)) # Randomly assign the first factor
  
  R <- rep(NA,N) # create empty vector of possible response statuses
  
  R[which(A1==+1)] <- rbinom(n=sum(A1==+1),
                             size=1,
                             prob=r_in_first_arm)   # Simulate response statuses for individuals given A1==+1
  R[which(A1==-1)] <- rbinom(n=sum(A1==-1),
                             size=1,
                             prob=r_in_second_arm)   # Simulate response statuses for individuals given A1==-1
  
  hidden_A2 <- rep(NA,N)  
  # The simulation will give everyone a potential value of A2, even though not everyone
  # is actually considered to be rerandomized.  This value can be imagined as
  # latent, counterfactual, or hidden in a sealed envelope.  For nonresponders,
  # it will be copied to their "real" A2 below.
  hidden_A2[which(A1==+1)] <- sample(level_codes,
                                     size=sum(A1==+1),
                                     replace=TRUE,
                                     prob=c(p2_in_first_arm,1-p2_in_first_arm))
  hidden_A2[which(A1==-1)] <- sample(level_codes,
                                     size=sum(A1==-1),
                                     replace=TRUE,
                                     prob=c(p2_in_second_arm,1-p2_in_second_arm))
  
  A2 <- rep(NA,N) # empty vector of possible second stage randomizations
  A2[which(R==0)] <- hidden_A2[which(R==0)]
  A2[which(R==1)] <- NA
  
  A2_with_zeros <- A2
  A2_with_zeros[which(R==1)] <- 0 # This version of A2 uses 0 instead of NA for responders.
  
  E_Y <-  beta_0 +
    beta_a1 * A1 +
    beta_a2 * A2_with_zeros +
    beta_ixn * A1 * A2_with_zeros +
    corr_eff * R  # Calculates expected value of Y conditional on treatment and R.
              # For the expected value conditional on embedded adaptive 
              # intervention only, you'd leave out the R term (i.e., set corr_eff to 0).
  Y <- round(E_Y + 
               rnorm(n=N,sd=sigma),
             decimal_places)  # Add error to E(Y) to get Y.
  sim_data <- data.frame(  id=1:N,
                           A1,
                           A2,  
                           A2_with_zeros,
                           R,
                           Y,
                           # The following variables wouldn't be directly observed in real life:;
                           hidden_A2, 
                           E_Y)
       # Package the simulated data into a data frame.
  return(sim_data)   # Return the data frame.
}