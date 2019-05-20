################################################################################
# File: 		analysis.R
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
library(DescTools)
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
comp_chi <- c("Global", "Prosecutors vs. Defense Attorneys", 
              "Prosecutors vs. Judges", "Defense Attorneys vs. Judges")
questions <- c("The proper role of NVRA in sentencing",
               "Familiarity with NVRA",
               "How often NVRA used",
               "Rely on legal (judicial) experience or NVRA",
               "Current availability of alternative sentencing options",
               "Sentencing practices changed by increased options")
chi_names <- c("Item", "Test", "Test statistic", "p-value")
ord_names <- c("Item", "Test", "Mean (1)", "Mean (2)", "Difference", 
               "p-value")

###  Replicate -----------------------------------------------------------------
colnums <- c(1:6)
sink("~/git/monahan_risk_attorney/output/replicate.txt")
for (i in colnums) {
  print(crosstab(df2[, i], df2[,7], plot = FALSE))
}
sink()

### Chi-Square Tests -----------------------------------------------------------
colnums <- c(1:6)
nsims <- 10000
set.seed(8675309)

for (i in colnums) {
  #set datasets
  df_full <- df2
  df_1v2 <- filter(df2, Simple_Group != "Judges")
  df_1v3 <- filter(df2, Simple_Group != "Defense Attorneys")
  df_2v3 <- filter(df2, Simple_Group != "Prosecutors")
  
  #set p-value inflator for bonferroni
  bon_n <- 3
  bon_alpha <- 1 - (1 -.05)^(1/3)
  
  #run tests
  t <- chisq.test(df_full[, i], df_full[, 7], simulate.p.value = TRUE, 
                  B = nsims)
  t_1v2 <- chisq.test(df_1v2[, i], df_1v2[, 7], simulate.p.value = TRUE, 
                    B = nsims)
  t_1v3 <- chisq.test(df_1v3[, i], df_1v3[, 7], simulate.p.value = TRUE, 
                    B = nsims)
  t_2v3 <- chisq.test(df_2v3[, i], df_2v3[, 7], simulate.p.value = TRUE, 
                    B = nsims)

  #table components
  item <- c(paste0("Q", i, ". ", questions[i]), "", "", "")
  test <- comp_chi
  chisq <- c(format(round(as.numeric(t$statistic), 2), nsmall = 2),
             format(round(as.numeric(t_1v2$statistic), 2), nsmall = 2),
             format(round(as.numeric(t_1v3$statistic), 2), nsmall = 2),
             format(round(as.numeric(t_2v3$statistic), 2), nsmall = 2))
  pval <- c(as.numeric(t$p.value), 
            as.numeric(t_1v2$p.value),
            as.numeric(t_1v3$p.value),
            as.numeric(t_2v3$p.value))
  pval <- addStars(pval)
  
  #assemble table
  x <- data.frame(item, test, chisq, pval)
  if (i == 1) {
    chi <- x
  }
  else {
    chi <- rbind(chi, x)
  }
}
## Print Table ##
names(chi) <- chi_names
stargazer(chi, type = "html", title = "Views of NVRA Sentencing: Post-Hoc Comparisons",
          summary = FALSE, rownames = FALSE,
          notes = paste("*p<0.05; **p<0.01; ***p<0.001",
                        "Chi-square tests with p-values sampled from 10,000 Monte Carlo simulations.",
                        "Bonferroni-corrected p-values for all post-hoc comparisons."),
          out = "~/git/monahan_risk_attorney/reports/chisq_posthoc.htm")


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
          notes = paste("*p<0.05; **p<0.01; ***p<0.001",
                        "Global tests assessed using Kruskal-Wallis rank sum test.",
                        "Post hoc tests assessed using Mann-Whitney test with Bonferroni-corrected p-values."),
          out = "~/git/monahan_risk_attorney/reports/kruskal_posthoc.htm")
## Ordered Logit Regression with Tukey Post-Hoc Comparisons ##
# colnums <- c(2:5)
# results_ologit <- vector("list", 6)
# for (i in colnums) {
#   lt <- leveneTest(as.numeric(df[,i]), df[,6])
#   mod <-  polr(as.formula(paste(colnames(df)[i], "~ Simple_Group")), data = df,
#                Hess = TRUE)
#   tukey <- glht(mod, mcp(Simple_Group = "Tukey"))
#   results_ologit[[i]] <- list(lt, summary(tukey), summary(mod), mod, tukey)
# }

