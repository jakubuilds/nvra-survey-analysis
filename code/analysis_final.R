################################################################################
# File: 		    analysis_final.R
#
# Description:	global chi square and post-hoc comparison tests
#
# Created by: 	Alex
# requires:     data_in/Risk_survey_judges.csv
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
df <- read.csv("data_in/Risk_survey_judges_revised.csv", 
               stringsAsFactors = FALSE)

## Convert to Factors ##
df$Simple_Group <- factor(df$Simple_Group,
                          levels = c(1, 2, 3),
                          labels = c("Prosecutors", "Defense Attorneys", 
                                     "Judges"))
df2 <- df
df2$Risk_attitude <- factor(df2$Risk_attitude,
                          levels = c(1, 2, 3),
                          labels = c("NVRA should play no role",
                                     "NVRA should play a role",
                                     "Other"))
df2$Familiarity <- ordered(df2$Familiarity,
                         levels = c(4, 3, 2, 1),
                         labels = c("Very familiar", "Familiar",
                                    "Slightly unfamiliar", "Unfamiliar"))
df2$Consider <- ordered(df2$Consider,
                          levels = c(5, 4, 3, 2, 1),
                          labels = c("Always/Almost always", "Usually",
                                     "Sometimes", "Rarely", "Never"))
df2$Sentence_logic <- ordered(df2$Sentence_logic,
                             levels = c(1, 2, 3, 4),
                             labels = c("Primarily NVRA",
                                        "Equally NVRA and legal (judicial) experience",
                                        "Primarily legal (judicial) experience",
                                        "No role for risk"))
df2$Availability <- ordered(df2$Availability,
                             levels = c(4, 3, 2, 1),
                             labels = c("Excellent", "Adequate",
                                        "Less than adequate",
                                        "Virtually non-existent"))
df2$More_AP <- factor(df2$More_AP,
                          levels = c(3, 2, 1),
                          labels = c("Yes", "No", "Do not know"))

## Table Info ##
comp <- c("Global", "Prosecutors (1) - Defense Attorneys (2)", 
          "Prosecutors  (1) - Judges (2)", 
          "Defense Attorneys (1) - Judges (2)")
questions <- c("The proper role of NVRA in sentencing",
               "Familiarity with NVRA",
               "How often NVRA used",
               "Rely on legal (judicial) experience or NVRA",
               "Current availability of alternative sentencing options",
               "Sentencing practices changed by increased options")
chi_names <- c("Item", "Test", "p-value")
sig <- "*p<0.05; **p<0.01; ***p<0.001"


###  Replicate -----------------------------------------------------------------
colnums <- c(1:6)
sink("~/git/monahan_risk_attorney/output/replicate.txt")
for (i in colnums) {
  print(crosstab(df2[, i], df2[,7], plot = FALSE))
}
sink()


### Chi-Square Tests: Global ---------------------------------------------------
## Priors ##
colnums <- c(1:6)
ttitle <- "Views of Non-Violent Risk Assessment (NVRA) in Sentencing"
sig <- "*p<0.05; **p<0.01; ***p<0.001"

## Run Chi-Squares ##
df_chi <- myChiSquare(df2, c(1:6), 7)
df_fish <- myChiSquare(df2, c(1:6), 7, fisher = TRUE, wrksp = 1e9)

## Table ##
stargazer(df_chi, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/chisq.htm")

stargazer(df_fish, type = "html", 
          title = ttitle,
          summary = FALSE, rownames = FALSE,
          notes = sig,
          out = "~/git/monahan_risk_attorney/reports/fish.htm")



### Chi-Square Tests: Post-Hoc -------------------------------------------------
colnums <- c(1:6)
set.seed(8675309)
tests <- c("chisq", "fisher")
for (j in 1:length(tests)) {
  for (i in colnums) {
    #table
    x <- table(df2$Simple_Group, df2[,i])
    
    #global
    tchi <- chisq.test(x, correct = FALSE)
    if (tests[j] == "fisher") {
      t <- fisher.test(x, workspace = 1e9)
      tchi$p.value <- t$p.value
    }

    t_global <- data.frame(item = paste0("Q", i, ". ", questions[i]), 
                           test = "Global",
                           pval = paste0(format(round(tchi$statistic, 1), nsmall = 1),
                                         gsub("^[0-9]\\.[0-9]*", "", 
                                              addStars(tchi$p.value))))
    t_global <- mutate_all(t_global, as.character)
    #post-hocs
    t_fwer <- chisq.post.hoc(x, test = paste0(tests[j], ".test"), control = "holm")
    t_fwer$adj.p <- ifelse(t_fwer$adj.p == 0.0000, 0.00025, t_fwer$adj.p)
    t_post <- data.frame(item = rep("", 3),
                         test = t_fwer$comparison,
                         pval = paste0(format(round(qchisq(t_fwer$adj.p, 1, lower.tail = FALSE), 1), 
                                              nsmall = 1),
                                       gsub("^[0-9]\\.[0-9]*", "", 
                                            addStars(t_fwer$adj.p))))
    t_post <- mutate_all(t_post, as.character)
  
    #post-hoc: control fdr
    t_out <- rbind(t_global, t_post)
  
    if (i == 1) {
      chi <- t_out
    }
    else {
      chi <- rbind(chi, t_out)
    }
  }
  
  ## Print Table ##
  names(chi) <- chi_names
  stargazer(chi, type = "html", title = "Views of NVRA Sentencing: Omnibus and Corrected Post-Hoc Comparisons",
            summary = FALSE, rownames = FALSE,
            notes = paste("Post-hoc tests use Holm-Bonferroni correction to control for familywise error rate.",
                          sig),
            out = paste0("~/git/monahan_risk_attorney/reports/", tests[j] ,"_posthoc.htm"))
}
