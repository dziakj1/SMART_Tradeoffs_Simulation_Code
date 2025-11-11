source("effect-size-formulas-sequentially-randomized-trial.R")
source("sample-size-formulas-sequentially-randomized-trial.R")
source("power-formulas-sequentially-randomized-trial.R")

###############################################################################
# Fix the following params: type-1 error rate: 0.05, power = 0.80
###############################################################################

all_sample_size <- c(350, 550)
all_prob <- list(`1` = c(1/2, 1/2), `2` = c(1/3, 1/2), `3` = c(4/10, 1/2))
my_grid <- expand.grid(all_sample_size = all_sample_size, all_prob = all_prob)
my_grid[["all_prob_stage1"]] <- lapply(my_grid$all_prob, FUN = function(x){x[1]})
my_grid[["all_prob_stage2"]] <- lapply(my_grid$all_prob, FUN = function(x){x[2]})
my_grid[["all_prob_stage1"]] <- unlist(my_grid[["all_prob_stage1"]])
my_grid[["all_prob_stage2"]] <- unlist(my_grid[["all_prob_stage2"]])

list_results <- list()

for(i in 1:nrow(my_grid)){
  current_sample_size <- my_grid[["all_sample_size"]][i]
  my_prob_stage1 <- my_grid[["all_prob_stage1"]][i]
  my_prob_stage2 <- my_grid[["all_prob_stage2"]][i]
  
  delta_1 <- GetEffectSizeStage1(type1_error_rate = 0.05, 
                                 sample_size = current_sample_size, 
                                 power_test = 0.80,
                                 prob_stage1 = my_prob_stage1)
  
  delta_2 <- GetEffectSizeStage2(type1_error_rate = 0.05, 
                                 sample_size = current_sample_size, 
                                 power_test = 0.80,
                                 prob_stage1 = my_prob_stage1,
                                 prob_stage2_given_0 = my_prob_stage2)
  
  delta_AB <- GetEffectSizeDifferentStages(type1_error_rate = 0.05, 
                                           sample_size = current_sample_size, 
                                           power_test = 0.80,
                                           prob_stage1 = my_prob_stage1,
                                           prob_stage2_given_0 = my_prob_stage2,
                                           a2 = 1)
  
  delta_AC <- GetEffectSizeDifferentStages(type1_error_rate = 0.05, 
                                           sample_size = current_sample_size, 
                                           power_test = 0.80,
                                           prob_stage1 = my_prob_stage1,
                                           prob_stage2_given_0 = my_prob_stage2,
                                           a2 = 0)
  
  list_results <- append(list_results,
                         list(data.frame(current_sample_size = current_sample_size,
                                         my_prob_stage1 = my_prob_stage1,
                                         my_prob_stage2 = my_prob_stage2,
                                         delta_1 = delta_1,
                                         delta_2 = delta_2,
                                         delta_AB = delta_AB,
                                         delta_AC = delta_AC)))
}

dat_all_results <- do.call(rbind, list_results)

dat_all_results <- dat_all_results[order(dat_all_results[["current_sample_size"]]),]

print(dat_all_results)

# > print(dat_all_results)
# current_sample_size my_prob_stage1 my_prob_stage2   delta_1   delta_2  delta_AB  delta_AC
# 1                 350      0.5000000            0.5 0.2995021 0.4235599 0.3668136 0.3668136
# 3                 350      0.3333333            0.5 0.3176699 0.3668136 0.3668136 0.3668136
# 5                 350      0.4000000            0.5 0.3056780 0.3866555 0.3616831 0.3616831
# 2                 550      0.5000000            0.5 0.2389200 0.3378839 0.2926160 0.2926160
# 4                 550      0.3333333            0.5 0.2534129 0.2926160 0.2926160 0.2926160
# 6                 550      0.4000000            0.5 0.2438467 0.3084444 0.2885233 0.2885233

###############################################################################
# Fix the following params: type-1 error rate: 0.05, power = 0.80
###############################################################################

all_effect_size <- c(0.2, 0.3, 0.5, 0.8)
all_prob <- list(`1` = c(1/2, 1/2), `2` = c(1/3, 1/2), `3` = c(4/10, 1/2))
my_grid <- expand.grid(all_effect_size = all_effect_size, all_prob = all_prob)
my_grid[["all_prob_stage1"]] <- lapply(my_grid$all_prob, FUN = function(x){x[1]})
my_grid[["all_prob_stage2"]] <- lapply(my_grid$all_prob, FUN = function(x){x[2]})
my_grid[["all_prob_stage1"]] <- unlist(my_grid[["all_prob_stage1"]])
my_grid[["all_prob_stage2"]] <- unlist(my_grid[["all_prob_stage2"]])

list_results <- list()

for(i in 1:nrow(my_grid)){
  current_effect_size <- my_grid[["all_effect_size"]][i]
  my_prob_stage1 <- my_grid[["all_prob_stage1"]][i]
  my_prob_stage2 <- my_grid[["all_prob_stage2"]][i]
  
  sample_size_stage1 <- GetSampleSizeStage1(standardized_effect_size = current_effect_size,
                                            type1_error_rate = 0.05, 
                                            power_test = 0.80,
                                            prob_stage1 = my_prob_stage1)
  
  sample_size_stage2 <- GetSampleSizeStage2(standardized_effect_size = current_effect_size,
                                            type1_error_rate = 0.05, 
                                            power_test = 0.80,
                                            prob_stage1 = my_prob_stage1,
                                            prob_stage2_given_0 = my_prob_stage2)
  
  sample_size_AB <- GetSampleSizeDifferentStages(standardized_effect_size = current_effect_size,
                                                 type1_error_rate = 0.05, 
                                                 power_test = 0.80,
                                                 prob_stage1 = my_prob_stage1,
                                                 prob_stage2_given_0 = my_prob_stage2,
                                                 a2 = 1)
  
  sample_size_AC <- GetSampleSizeDifferentStages(standardized_effect_size = current_effect_size,
                                                 type1_error_rate = 0.05, 
                                                 power_test = 0.80,
                                                 prob_stage1 = my_prob_stage1,
                                                 prob_stage2_given_0 = my_prob_stage2,
                                                 a2 = 0)
  
  list_results <- append(list_results,
                         list(data.frame(current_effect_size = current_effect_size,
                                         my_prob_stage1 = my_prob_stage1,
                                         my_prob_stage2 = my_prob_stage2,
                                         sample_size_stage1 = sample_size_stage1,
                                         sample_size_stage2 = sample_size_stage2,
                                         sample_size_AB = sample_size_AB,
                                         sample_size_AC = sample_size_AC)))
}

dat_all_results <- do.call(rbind, list_results)

dat_all_results <- dat_all_results[order(dat_all_results[["current_effect_size"]]),]

dat_all_results[,4:7] <- round(dat_all_results[,4:7], 0)

print(dat_all_results)

# > print(dat_all_results)
# current_effect_size my_prob_stage1 my_prob_stage2 sample_size_stage1 sample_size_stage2 sample_size_AB sample_size_AC
# 1                  0.2      0.5000000            0.5                785               1570           1177           1177
# 5                  0.2      0.3333333            0.5                883               1177           1177           1177
# 9                  0.2      0.4000000            0.5                818               1308           1145           1145
# 2                  0.3      0.5000000            0.5                349                698            523            523
# 6                  0.3      0.3333333            0.5                392                523            523            523
# 10                 0.3      0.4000000            0.5                363                581            509            509
# 3                  0.5      0.5000000            0.5                126                251            188            188
# 7                  0.5      0.3333333            0.5                141                188            188            188
# 11                 0.5      0.4000000            0.5                131                209            183            183
# 4                  0.8      0.5000000            0.5                 49                 98             74             74
# 8                  0.8      0.3333333            0.5                 55                 74             74             74
# 12                 0.8      0.4000000            0.5                 51                 82             72             72
