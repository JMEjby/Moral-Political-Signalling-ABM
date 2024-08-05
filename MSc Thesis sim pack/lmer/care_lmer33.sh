#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N lmer33_care
#$ -M s1917169@ed.ac.uk
#$ -cwd
#$ -l h_rt=6:00:00
#$ -l h_vmem=5G

# Initialise the environment modules
. /etc/profile.d/modules.sh
module load R/4.3.0

Rscript care_lmer_33.R
