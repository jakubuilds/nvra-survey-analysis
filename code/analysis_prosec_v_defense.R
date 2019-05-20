################################################################################
# File: 		    analysis_prosec_v.defense.R
#
# Description:	crosstabs and chi-square tests for supplemental questions
#               comparing prosecutors and defense attorneys only
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

## Convert to Factors ##
likert_labs1 <- c("Never", "Rarely", "Sometimes", "Usually", 
                  "Always/Almost always")
likert_labs2 <- c("Strongly disagree", "Disagree", "No opinion", "Agree",
                  "Strongly Agree")
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


## Table Info ##
questions <- c("How often NVRA results made available before plea negotiations",
               "How often NVRA used during plea negotiations",
               "Should NVRA results be made available before plea negotiations")


###  Chi-Square  ---------------------------------------------------------------
## Priors ##
ttitle <- "Views of NVRA Sentencing: Prosecutor vs. Defense Attorney Questions"
sig <- "*p<0.05; **p<0.01; ***p<0.001"
## Run Chi-Squares ##
df_chi <- myChiSquare(df, c(1:3), 4)
df_fish <- myChiSquare(df, c(1:3), 4, fisher = TRUE)

## Table ##
stargazer(df_chi, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/chisq_pro_v_att.htm")

stargazer(df_fish, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/fish_pro_v_att.htm")


### Ordinal Comparisons --------------------------------------------------------
## Priors ##
colnums <- c(1:3)
group_col <- 4
ord_names <- c("Item", "Prosecutors", "Defense Attorneys", "Difference", 
               "p-value")

## Grab Means ##
means <- df %>%
  group_by(Prosecutor) %>%
  mutate_at(vars(Have_results:Should_results_be_available), 
            funs(as.numeric)) %>%
  summarize_at(vars(Have_results:Should_results_be_available), 
               funs(mean(., na.rm = TRUE)))

## Loop Kruskal Tests ##
for (i in colnums) {
  Item <- questions[i]
  global <- kruskal.test(as.numeric(df[,i]), as.numeric(df[,group_col]))
  mean1 <- as.numeric(means[1,i])
  mean2 <- as.numeric(means[2,i])
  diff <- mean1 - mean2
  pval <- c(global$p.value)
  pval <- addStars(pval)
  
  x <- data.frame(Item,
                  mean1 = format(round(mean1, 2), nsmall = 2),
                  mean2 = format(round(mean2, 2), nsmall = 2),
                  diff = format(round(diff, 2), nsmall = 2),
                  pval = pval)
  if (i == colnums[1]) {
    ord <- x
  }
  else {
    ord <- rbind(ord, x)
  }
}
names(ord) <- ord_names

stargazer(ord, type = "html", title = "Views of NVRA Sentencing: Prosecutor vs. Defense Attorneys Ordinal Response Questions",
          summary = FALSE, rownames = FALSE,
          notes = paste(sig,
                        "Global test assessed using Kruskal-Wallis rank sum test."),
          out = "~/git/monahan_risk_attorney/reports/kruskal_pro_v_att.htm")
