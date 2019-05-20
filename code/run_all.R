################################################################################
# File: 		run_all.R
#
# Description:  master script file for all scripts in this project
#
# Created by:	Alex
# requires:     ...
# provides:     ...
################################################################################


### Priors ---------------------------------------------------------------------
rm(list = ls())
rootdir <- "~/alex.jakubow@gmail.com/work/projects/monahan_attorney_risk/"


### Execute --------------------------------------------------------------------
source(paste0(rootdir, "analysis_final.R"))  
source(paste0(rootdir, "analysis_pro_v_att_final.R"))  
