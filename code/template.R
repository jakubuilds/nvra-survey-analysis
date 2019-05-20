################################################################################
# File:         
#
# Description:  
#
# Created by:	  Alex Jakubow
# requires:     
# provides:     
################################################################################


### Priors ---------------------------------------------------------------------
rm(list = ls())
setwd("~/work/projects/")

# packages
library(dplyr)

# load functions
sapply(list.files("functions", pattern = "*.R", full.names = TRUE), source)
