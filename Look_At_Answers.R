rm(list = ls())
library(dplyr)
library(ggplot2)
load("ran-many-simulations-2025-10-24.rdata") 
all_sims <- cbind(scenarios, answers_data)
all_sims <- all_sims %>% mutate(balanced_alloc_stage_1 = p1==.5,
                                scenario_1 = case_when((equal_R==TRUE) | (corr_eff==0) ~ "No extra path",
                                                       (equal_R==FALSE) & (corr_eff!=0) ~ "Extra path"),
                                scenario_2 = case_when(((equal_R==TRUE) & (balanced_alloc_stage_1 ==TRUE) ) ~ "Orthogonal factors",
                                                       ((equal_R==FALSE) | (balanced_alloc_stage_1 ==FALSE) ) ~ "Nonorthogonal factors"),
                                scenario_3 = balanced_alloc_stage_1
)
plot1a <- ggplot(data=all_sims,
                 mapping=aes(x=formula_power_a1, 
                             y=simulated_power_a1,
                             color=scenario_1 
                 )) + geom_point() + 
  xlab("Formula")+
  ylab("Simulated")+ 
  xlim(0,1)+
  ylim(0,1)+
  scale_x_continuous(breaks = (0:5)/5)+
  scale_y_continuous(breaks = (0:5)/5)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal() +
  coord_fixed()  + 
  labs(title="Main Effect for Stage 1",
       subtitle="Dots are simulation scenarios."  ) +
  theme(axis.line = element_line(colour = "black"))
plot(plot1a,
     main="Power for Stage 1 Main Effect")
## Whether the power formula works correctly for 
## the first-stage main effect depends on whether
## an unmodeled A1->R->Y path causes the conditional
## and marginal effects of A1 to differ.  This 
## happens when the response rates are unequal and
## there is also a correlational effect of R and Y.


plot2a <- ggplot(data=all_sims,
                 mapping=aes(x=formula_power_a2, 
                             y=simulated_power_a2,
                             color=scenario_2
                 )) + geom_point() + 
  xlab("Formula")+
  ylab("Simulated")+ 
  xlim(0,1)+
  ylim(0,1)+
  scale_x_continuous(breaks = (0:5)/5)+
  scale_y_continuous(breaks = (0:5)/5)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal() +
  coord_fixed()  + 
  labs(title="Main Effect for Stage 2",
       subtitle="Dots are simulation scenarios."  ) +
  theme(axis.line = element_line(colour = "black") )
plot(plot2a)


plot3a <- ggplot(data=all_sims,
                 mapping=aes(x=formula_power_pp_vs_pm, 
                             y=simulated_power_pp_vs_pm,
                             color=as.factor(beta_ixn)
                 )) + geom_point() + 
  xlab("Formula")+
  ylab("Simulated")+ 
  xlim(0,1)+
  ylim(0,1)+
  scale_x_continuous(breaks = (0:5)/5)+
  scale_y_continuous(breaks = (0:5)/5)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal() +
  coord_fixed()  + 
  labs(title="Pairwise for ++ versus +-  (1,1 vs. 1,0)"   ) +
  theme(axis.line = element_line(colour = "black") )
plot(plot3a)



plot4a <- ggplot(data=all_sims %>% filter(beta_ixn ==0),
                 mapping=aes(x=formula_power_pp_vs_mp, 
                             y=simulated_power_pp_vs_mp,
                             color=equal_R
                 )) + geom_point() + 
  xlab("Formula")+
  ylab("Simulated")+ 
  xlim(0,1)+
  ylim(0,1)+
  scale_x_continuous(breaks = (0:5)/5)+
  scale_y_continuous(breaks = (0:5)/5)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal() +
  coord_fixed()  + 
  labs(title="Pairwise for ++ versus -+  (1,1 vs. 0,1)" ) +
  theme(axis.line = element_line(colour = "black") )
plot(plot4a)




plot4b <- ggplot(data=all_sims %>% filter(equal_R ==FALSE,
                                          beta_ixn==0),
                 mapping=aes(x=formula_power_pp_vs_mp, 
                             y=simulated_power_pp_vs_mp,
                             color=as.factor(scenario_1)
                 )) + geom_point() + 
  xlab("Formula")+
  ylab("Simulated")+ 
  xlim(0,1)+
  ylim(0,1)+
  scale_x_continuous(breaks = (0:5)/5)+
  scale_y_continuous(breaks = (0:5)/5)+
  geom_abline(intercept = 0, slope = 1)+
  theme_minimal() +
  coord_fixed()  + 
  labs(title="Pairwise for ++ versus -+  (1,1 vs. 0,1)",
       subtitle="Only Unequal R cases with beta ixn=0") +
  theme(axis.line = element_line(colour = "black") )
plot(plot4b)


