################################################################################
# File: 		analysis_revised.R
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
df <- read.csv("data_in/Risk_survey_judges.csv", stringsAsFactors = FALSE)

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
chi_names <- c("Item", "Test", "p-value", "p-value (FWER)", 
               "p-value (FDR)")
ord_names("Item", "Test", "Mean (1)", "Mean (2)", "Difference", "p-value")
sig <- "*p<0.05; **p<0.01; ***p<0.001"

###  Replicate -----------------------------------------------------------------
colnums <- c(1:6)
sink("~/git/monahan_risk_attorney/output/replicate.txt")
for (i in colnums) {
  print(crosstab(df2[, i], df2[,7], plot = FALSE))
}
sink()


### Chi-Square Tests -----------------------------------------------------------
colnums <- c(1:6)
nsims <- 100000
set.seed(8675309)
tests <- c("chisq", "fisher")
for (j in 1:length(tests)) {
  for (i in colnums) {
    #table
    x <- table(df2$Simple_Group, df2[,i])
    
    #global
    if (tests[j] == "chisq") {
      t <- chisq.test(x, correct = FALSE)
    }
    else {
      t <- fisher.test(x, workspace = 1e9)
    }
    t_global <- data.frame(item = paste0("Q", i, ". ", questions[i]), 
                           test = "Global",
                           pval = addStars(t$p.value),
                           fwer = NA,
                           fdr = NA)
    #post-hocs
    t_fwer <- chisq.post.hoc(x, test = paste0(tests[j], ".test"), control = "holm")
    t_fdr <- chisq.post.hoc(x, test =paste0(tests[j], ".test"),  control = "fdr")
    t_post <- data.frame(item = rep("", 3),
                         test = t_fwer$comparison,
                         pval = rep(NA, 3),
                         fwer = addStars(t_fwer$adj.p),
                         fdr = addStars(t_fdr$adj.p))
  
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
  stargazer(chi, type = "html", title = "Views of NVRA Sentencing: Post-Hoc Comparisons",
            summary = FALSE, rownames = FALSE,
            notes = paste("FWER: corrected p-values to minimize familywise error rate using Holm-Bonferroni correction",
                          "FDR: corrected p-values to minimize false discovery rate using Benjamin-Hochberg correction",
                          sig),
            out = paste0("~/git/monahan_risk_attorney/reports/", tests[j] ,"_posthoc.htm"))
}

### Ordinal Comparisons --------------------------------------------------------
## Grab Means ##
means <- df %>%
  group_by(Simple_Group) %>%
  summarize_at(vars(Risk_attitude:More_AP), funs(mean(., na.rm = TRUE)))

## Loop Kruskal Tests ##
colnums <- c(2:5)
for (i in colnums) {
  Item <- c(paste0("Q", i, ". ", questions[i]), "", "", "")
  global <- kruskal.test(df[,i], df$Simple_Group)
  pwise <- pairwise.wilcox.test(df[,i], df$Simple_Group,
                                p.adjust.method = "bonferroni")
  mean1 <- unlist(c(means[1,i+1], means[1,i+1], means[2,i+1]))
  mean2 <- unlist(c(means[2,i+1], means[3,i+1], means[3,i+1]))
  diff <- mean1 - mean2
  pval <- c(global$p.value, pwise$p.value[1,1], pwise$p.value[2,1],
            pwise$p.value[2,2])
  pval <- addStars(pval)
  
  x <- data.frame(Item, comp, 
                  mean1 = c("--", format(round(mean1, 2), nsmall = 2)),
                  mean2 = c("--", format(round(mean2, 2), nsmall = 2)),
                  diff = c("--", format(round(diff, 2), nsmall = 2)),
                  pval = c(pval))
  if (i == 2) {
    ord <- x
  }
  else {
    ord <- rbind(ord, x)
  }
}
names(ord) <- ord_names

stargazer(ord, type = "html", title = "Views of NVRA Sentencing: Post-Hoc Comparisons of Ordinal Response Questions",
          summary = FALSE, rownames = FALSE,
          notes = paste(sig,
                        "Global tests assessed using Kruskal-Wallis rank sum test.",
                        "Post hoc tests assessed using Mann-Whitney test with Bonferroni-corrected p-values."),
          out = "~/git/monahan_risk_attorney/reports/kruskal_posthoc.htm")
