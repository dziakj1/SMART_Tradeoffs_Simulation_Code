source("effect-size-formulas-prototypical-SMART.R")
source("sample-size-formulas-prototypical-SMART.R")
source("power-formulas-prototypical-SMART.R")

###############################################################################
# Fix response rates to 0.40 or 0.60
# Fix the following params: type-1 error rate: 0.05, power = 0.80
###############################################################################

all_response_prob <- c(0.40, 0.60)
all_sample_size <- c(350, 600, 900, 1200, 1750)
all_params <- expand.grid(all_response_prob = all_response_prob,
                          all_sample_size = all_sample_size)
dat_all_results <- data.frame(all_params)
dat_all_results[["delta_1"]] <- NA
dat_all_results[["delta_2"]] <- NA
dat_all_results[["delta_s_early"]] <- NA
dat_all_results[["delta_s_late"]] <- NA
dat_all_results[["delta_d"]] <- NA

for(i in 1:nrow(dat_all_results)){
  current_response_prob <- dat_all_results[["all_response_prob"]][i]
  current_sample_size <- dat_all_results[["all_sample_size"]][i]
  
  delta_1 <- GetEffectSizeStage1(type1_error_rate = 0.05, 
                                 sample_size = current_sample_size, 
                                 power_test = 0.80,
                                 prob_stage1 = 0.50)
  
  delta_2 <- GetEffectSizeStage2WhenEqual(type1_error_rate = 0.05, 
                                          sample_size = current_sample_size, 
                                          power_test = 0.80, 
                                          r = current_response_prob)
  
  delta_s_early <- GetEffectSizeSameStartADI(type1_error_rate = 0.05, 
                                             sample_size = current_sample_size, 
                                             power_test = 0.80, 
                                             prob_stage1 = 0.50, 
                                             response_1 = current_response_prob, 
                                             response_0 = current_response_prob, 
                                             prob_stage2_given_1 = 0.50, 
                                             prob_stage2_given_0 = 0.50, 
                                             a1 = 1)
  
  delta_s_late <- GetEffectSizeSameStartADI(type1_error_rate = 0.05, 
                                            sample_size = current_sample_size, 
                                            power_test = 0.80, 
                                            prob_stage1 = 0.50, 
                                            response_1 = current_response_prob, 
                                            response_0 = current_response_prob, 
                                            prob_stage2_given_1 = 0.50, 
                                            prob_stage2_given_0 = 0.50, 
                                            a1 = 0)
  
  delta_d <- GetEffectSizeDifferentStartADI(type1_error_rate = 0.05, 
                                            sample_size = current_sample_size, 
                                            power_test = 0.80, 
                                            prob_stage1 = 0.50, 
                                            response_1 = current_response_prob, 
                                            response_0 = current_response_prob, 
                                            prob_stage2_given_1 = 0.50, 
                                            prob_stage2_given_0 = 0.50, 
                                            a2_given_1 = 1, 
                                            b2_given_0 = 0)
  
  dat_all_results[["delta_1"]][i] <- delta_1
  dat_all_results[["delta_2"]][i] <- delta_2
  dat_all_results[["delta_s_early"]][i] <- delta_s_early
  dat_all_results[["delta_s_late"]][i] <- delta_s_late
  dat_all_results[["delta_d"]][i] <- delta_d
}

dat_all_results <- dat_all_results[order(dat_all_results[["all_response_prob"]], dat_all_results[["all_sample_size"]]),]

print(dat_all_results)

# > print(dat_all_results)
# all_response_prob all_sample_size   delta_1   delta_2 delta_s_early delta_s_late   delta_d
# 1                0.4             350 0.2995021 0.3866555     0.5468134    0.5468134 0.3788435
# 3                0.4             600 0.2287485 0.2953130     0.4176357    0.4176357 0.2893465
# 5                0.4             900 0.1867723 0.2411221     0.3409981    0.3409981 0.2362504
# 7                0.4            1200 0.1617496 0.2088178     0.2953130    0.2953130 0.2045989
# 9                0.4            1750 0.1339414 0.1729176     0.2445424    0.2445424 0.1694239
# 2                0.6             350 0.2995021 0.4735543     0.6697070    0.6697070 0.3543756
# 4                0.6             600 0.2287485 0.3616831     0.5114971    0.5114971 0.2706588
# 6                0.6             900 0.1867723 0.2953130     0.4176357    0.4176357 0.2209920
# 8                0.6            1200 0.1617496 0.2557486     0.3616831    0.3616831 0.1913847
# 10               0.6            1750 0.1339414 0.2117799     0.2995021    0.2995021 0.1584816

###############################################################################
# Fix response rates to 0.40  or 0.60
# Fix the following params: type-1 error rate: 0.05, power = 0.80
###############################################################################

all_response_prob <- c(0.40, 0.60)
all_std_effect_size <- c(0.20, 0.30, 0.50, 0.80)
all_params <- expand.grid(all_response_prob = all_response_prob,
                          all_std_effect_size = all_std_effect_size)
dat_all_results <- data.frame(all_params)
dat_all_results[["sample_size_stage1"]] <- NA
dat_all_results[["sample_size_stage2"]] <- NA
dat_all_results[["sample_size_same_start_adi_early"]] <- NA
dat_all_results[["sample_size_same_start_adi_late"]] <- NA
dat_all_results[["sample_size_different_start_adi"]] <- NA

for(i in 1:nrow(dat_all_results)){
  current_response_prob <- dat_all_results[["all_response_prob"]][i]
  current_std_effect_size <- dat_all_results[["all_std_effect_size"]][i]
  
  sample_size_stage1 <- GetSampleSizeStage1(standardized_effect_size = current_std_effect_size,
                                            type1_error_rate = 0.05, 
                                            power_test = 0.80,
                                            prob_stage1 = 0.50)
  
  sample_size_stage2 <- GetSampleSizeStage2WhenEqual(standardized_effect_size = current_std_effect_size,
                                                     type1_error_rate = 0.05, 
                                                     power_test = 0.80,
                                                     r = current_response_prob)
  
  sample_size_same_start_adi_early <- GetSampleSizeSameStartADI(standardized_effect_size = current_std_effect_size,
                                                                type1_error_rate = 0.05, 
                                                                power_test = 0.80,
                                                                prob_stage1 = 0.50,
                                                                response_1 = current_response_prob,
                                                                response_0 = current_response_prob,
                                                                prob_stage2_given_1 = 0.50,
                                                                prob_stage2_given_0 = 0.50,
                                                                a1 = 1)
  
  sample_size_same_start_adi_late <- GetSampleSizeSameStartADI(standardized_effect_size = current_std_effect_size,
                                                               type1_error_rate = 0.05, 
                                                               power_test = 0.80,
                                                               prob_stage1 = 0.50,
                                                               response_1 = current_response_prob,
                                                               response_0 = current_response_prob,
                                                               prob_stage2_given_1 = 0.50,
                                                               prob_stage2_given_0 = 0.50,
                                                               a1 = 0)
  
  sample_size_different_start_adi <- GetSampleSizeDifferentStartADI(standardized_effect_size = current_std_effect_size,
                                                                    type1_error_rate = 0.05, 
                                                                    power_test = 0.80,
                                                                    prob_stage1 = 0.50,
                                                                    response_1 = current_response_prob,
                                                                    response_0 = current_response_prob,
                                                                    prob_stage2_given_1 = 0.50,
                                                                    prob_stage2_given_0 = 0.50,
                                                                    a2_given_1 = 1,
                                                                    b2_given_0 = 0)
  
  dat_all_results[["sample_size_stage1"]][i] <- sample_size_stage1
  dat_all_results[["sample_size_stage2"]][i] <- sample_size_stage2
  dat_all_results[["sample_size_same_start_adi_early"]][i] <- sample_size_same_start_adi_early
  dat_all_results[["sample_size_same_start_adi_late"]][i] <- sample_size_same_start_adi_late
  dat_all_results[["sample_size_different_start_adi"]][i] <- sample_size_different_start_adi
}

dat_all_results <- dat_all_results[order(dat_all_results[["all_response_prob"]], dat_all_results[["all_std_effect_size"]]),]

dat_all_results[, 3:ncol(dat_all_results)] <- round(dat_all_results[, 3:ncol(dat_all_results)], 0)

print(dat_all_results)

# > print(dat_all_results)
# all_response_prob all_std_effect_size sample_size_stage1 sample_size_stage2 sample_size_same_start_adi_early sample_size_same_start_adi_late sample_size_different_start_adi
# 1               0.4                 0.2                785               1308                             2616                            2616                            1256
# 3               0.4                 0.3                349                581                             1163                            1163                             558
# 5               0.4                 0.5                126                209                              419                             419                             201
# 7               0.4                 0.8                 49                 82                              164                             164                              78
# 2               0.6                 0.2                785               1962                             3924                            3924                            1099
# 4               0.6                 0.3                349                872                             1744                            1744                             488
# 6               0.6                 0.5                126                314                              628                             628                             176
# 8               0.6                 0.8                 49                123                              245                             245                              69

