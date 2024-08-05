.libPaths(c("/exports/eddie/scratch/s1917169/lmer/R_libs", .libPaths())) # change this to path/on/cluster/lmer/R_libs

library(lme4)
library(stringr)
require(broom.mixed)
require(lmeresampler)

loc <- "/exports/eddie/scratch/s1917169/lmer/" # change this as well
load(str_c(loc,"Mixed_model_data_33.Rdata")) 


m_care1 <- lmer(w_care ~ identity_bin*(c_tick + poly2) + 
                 (1 + identity_bin*(c_tick + poly2)|sim/u_who), 
               data = cp33_tab_lmer1, 
               control = lmerControl(optimizer = "bobyqa", 
                                     calc.derivs = FALSE,
                                     optCtrl = list(maxfun = 20000)))
save(m_care1, file = str_c(loc, "results/33_care_drift_model_s1.RData"))

aug_care1 <- augment(m_care1)
save(aug_care1, file = str_c(loc, "results/33_augment_care_drift_model_s1.RData"))

lme_cases_care1 <- bootstrap(m_care1, .f = fixef, type = "case", B = 1000, resample = c(F, T))
ci_care1 <- lme_cases_care1
save(ci_care1, file = str_c(loc, "results/33_CI_care_drift_model_s1.RData"))

rm(list=ls())


loc <- "/exports/eddie/scratch/s1917169/lmer/" # change this as well
load(str_c(loc,"Mixed_model_data2_33.Rdata"))

m_care2 <- lmer(w_care ~ identity_bin*(c_tick + poly2) + 
                 (1 + identity_bin*(c_tick + poly2)|sim/u_who), 
               data = cp33_tab_lmer2, 
               control = lmerControl(optimizer = "bobyqa", 
                                     calc.derivs = FALSE,
                                     optCtrl = list(maxfun = 20000)))
save(m_care2, file = str_c(loc, "results/33_care_drift_model_s2.RData"))

aug_care2 <- augment(m_care2)
save(aug_care2, file = str_c(loc, "results/33_augment_care_drift_model_s2.RData"))

lme_cases_care2 <- bootstrap(m_care2, .f = fixef, type = "case", B = 1000, resample = c(F, T))
ci_care2 <- lme_cases_care2
save(ci_care2, file = str_c(loc, "results/33_CI_care_drift_model_s2.RData"))