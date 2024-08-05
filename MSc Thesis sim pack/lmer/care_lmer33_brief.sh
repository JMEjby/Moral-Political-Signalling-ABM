#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N lmer33_care
#$ -M s1917169@ed.ac.uk
#$ -cwd
#$ -l h_rt=4:00:00
#$ -l h_vmem=5G

# Initialise the environment modules
. /etc/profile.d/modules.sh
module load R/4.3

R

library(stringr)
library(MatrixModels)
library(lme4)
library(lmeresampler)
library(broom.mixed)


load("/exports/eddie/scratch/s1917169/JP_lmer/Mixed_model_data_33.Rdata")
loc <- "/exports/eddie/scratch/s1917169/JP_lmer/results/"

m_care1 <- lmer(w_care ~ identity_bin*(c_tick + poly2) + 
                 (1 + identity_bin*(c_tick + poly2)|sim/u_who), 
               data = cp33_tab_lmer1, 
               control = lmerControl(optimizer = "bobyqa", 
                                     calc.derivs = FALSE,
                                     optCtrl = list(maxfun = 20000)))
save(m_care1, file = str_c(loc, "33_care_drift_model_s1.RData"))

aug_care1 <- augment(m_care1)
save(aug_care1, file = str_c(loc, "33_augment_care_drift_model_s1.RData"))

lme_cases_care1 <- bootstrap(m_care1, .f = fixef, type = "case", B = 1000, resample = c(F, T))
ci_care1 <- lme_cases_care1
save(ci_care1, file = str_c(loc, "33_CI_care_drift_model_s1.RData"))

rm(list=ls())

load("/exports/eddie/scratch/s1917169/JP_lmer/Mixed_model_data2_33.Rdata")
loc <- "/exports/eddie/scratch/s1917169/JP_lmer/results/"

m_care2 <- lmer(w_care ~ identity_bin*(c_tick + poly2) + 
                 (1 + identity_bin*(c_tick + poly2)|sim/u_who), 
               data = cp33_tab_lmer2, 
               control = lmerControl(optimizer = "bobyqa", 
                                     calc.derivs = FALSE,
                                     optCtrl = list(maxfun = 20000)))
save(m_care2, file = str_c(loc, "33_care_drift_model_s2.RData"))

aug_care2 <- augment(m_care2)
save(aug_care2, file = str_c(loc, "33_augment_care_drift_model_s2.RData"))

lme_cases_care2 <- bootstrap(m_care2, .f = fixef, type = "case", B = 1000, resample = c(F, T))
ci_care2 <- lme_cases_care2
save(ci_care2, file = str_c(loc, "33_CI_care_drift_model_s2.RData"))

quit()