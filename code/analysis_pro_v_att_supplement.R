################################################################################
# File: 		    analysis_pro_v_att_supplement.R
#
# Description:	crosstabs and chi-square tests for supplemental questions
#               comparing prosecutors and defense attorneys only.  categories
#               are collapsed for some questions in light of revision 
#               suggetions from JM on 04/18
# Created by: 	Alex Jakubow
# requires:     data_in/Questions7-9_ProDef.csv
# provides:       
################################################################################


### Priors ---------------------------------------------------------------------
rm(list = ls())
setwd("~/alex.jakubow@gmail.com/work/projects/monahan_risk_attorney")

## Packages ##
library(dplyr)
library(fifer)
library(multcomp)
library(descr)
library(stargazer)

## Functions ##
functions <- list.files("~/git/monahan_risk_attorney/functions/", 
                        pattern = "*.R", full.names = TRUE)
sapply(functions, function(x) {
  source(x)
})

### Priors --------------------------------------------------------------------
## Import Data ##
df <- read.csv("data_in/Questions7-9_ProDef.csv",  stringsAsFactors = FALSE)

## Collapse Categories
df$Have_results3 <- ifelse(df$Have_results %in% c(4, 5), 3, df$Have_results)
df$Have_results2 <- ifelse(df$Have_results %in% c(4, 5), 4, df$Have_results)
df$Results_Use3 <- ifelse(df$Results_Use %in% c(4, 5), 3, df$Results_Use)
df$Results_Use2 <- ifelse(df$Results_Use %in% c(4, 5), 4, df$Results_Use)


## Convert to Factors ##
#original labels
likert_labs1 <- c("Never", "Rarely", "Sometimes", "Usually", 
                  "Always/Almost always")
likert_labs2 <- c("Strongly disagree", "Disagree", "No opinion", "Agree",
                  "Strongly Agree")
#collapsed labels
likert_labs1a <- c("Never", "Rarely", "Sometimes+")
likert_labs1b <- c("Never", "Rarely", "Sometimes", "Usually+")

#original assignment
likert_vals <- c(1, 2, 3, 4, 5)
df$Prosecutor <- factor(df$Prosecutor,
                          levels = c(1, 2),
                          labels = c("Prosecutors", "Defense Attorneys"))
df$Have_results <- factor(df$Have_results,
                          levels = likert_vals,
                          labels = likert_labs1)
df$Results_Use <- factor(df$Results_Use,
                          levels = likert_vals,
                          labels = likert_labs1)
df$Should_results_be_available <- factor(df$Should_results_be_available,
                          levels = likert_vals,
                          labels = likert_labs2)
#collapsed assignment
df$Have_results3 <- factor(df$Have_results3,
                          levels = c(1, 2, 3),
                          labels = likert_labs1a)
df$Have_results2 <- factor(df$Have_results2,
                           levels = c(1, 2, 3, 4),
                           labels = likert_labs1b)
df$Results_Use3 <- factor(df$Results_Use3,
                         levels = c(1, 2, 3),
                         labels = likert_labs1a)
df$Results_Use2 <- factor(df$Results_Use2,
                          levels = c(1, 2, 3, 4),
                          labels = likert_labs1b)


## Table Info ##
questions <- c(NA,
               NA,
               NA,
               NA,
               "How often NVRA results made available before plea negotiations (collapse 3)",
               "How often NVRA results made available before plea negotiations (collapse 2)",
               "How often NVRA used during plea negotiations (collapse 3)",
               "How often NVRA used during plea negotiations (collapse 2)")


###  Chi-Square  ---------------------------------------------------------------
## Priors ##
ttitle <- "Views of NVRA Sentencing: Prosecutor vs. Defense Attorney Questions (Supplement)"
sig <- "*p<0.05; **p<0.01; ***p<0.001"
## Run Chi-Squares ##
df_chi <- myChiSquare(df, c(5:8), 4)
df_fish <- myChiSquare(df, c(5:8), 4, fisher = TRUE)

## Table ##
stargazer(df_chi, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/chisq_pro_v_att_supp.htm")

stargazer(df_fish, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/fish_pro_v_att_supp.htm")
