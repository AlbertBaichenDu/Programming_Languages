# POLI3148 Final Project
# Regression
# Author: Albert Baichen Du

library(tidyverse)
library(jmv)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Undergraduate/Year3/POLI3148/final_project")

flow_class <- read.csv('DV3_flow_class.csv')
intent <- read.csv('DV2_intent.csv')
fm_data <- read.csv('final.csv')

df1 <- left_join(flow_class, fm_data, by = c("Recipient" = 'name.y'))
df2 <- left_join(intent, fm_data, by = c("Recipient" = 'name.y'))

# flow_class
df1 <- rename(df1, flow_class = Flow.Class)
df1 <- rename(df1, freedom_score = freedomscore)
df1 <- rename(df1, corruption_score = corruptionscore)
df1 <- rename(df1, elderly_population = elderlypop)
df1 <- rename(df1, healthcare_expenditure = healthcare)
df1 <- rename(df1, government_transparency = govtrans)
df1 <- rename(df1, government_efficiency = goveff)
df1 <- rename(df1, spending_efficiency = spendeff)
df1 <- rename(df1, year_month_frequency = year_month_freq)
# Method 1: jmv
jmv::logRegBin(
  data = df1,
  dep = flow_class,
  covs = vars(polity, freedom_score, corruption_score, population, elderly_population, healthcare_expenditure, government_transparency, government_efficiency, spending_efficiency, year_month_frequency),
  blocks = list(
    list(
      "polity",
      "freedom_score",
      "corruption_score",
      "population",
      "elderly_population",
      "healthcare_expenditure",
      "government_transparency",
      "government_efficiency",
      "spending_efficiency",
      "year_month_frequency")),
  refLevels = list(
    list(
      var="flow_class",
      ref="Non-ODA")))

# Method2: Glm
df1$flow_class <- factor(df1$flow_class, exclude = c("", NA))
res <- glm(flow_class ~ polity + freedom_score + corruption_score 
           + elderly_population + population + healthcare_expenditure
           + government_transparency + government_efficiency + spending_efficiency + year_month_frequency,
           family = binomial,
           data = df1)
summary(res)
"Model coefficients and confidence intervals"
cbind(coef(res), confint(res, level = 0.95, method = "Wald"))

"Odds/Risk ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

car::infIndexPlot(res)
"Chan, G. and StatsNotebook Team (2020). StatsNotebook. (Version 0.1.2) [Computer Software]. Retrieved from https://www.statsnotebook.io"
"R Core Team (2020). The R Project for Statistical Computing. [Computer software]. Retrieved from https://r-project.org"


# Intent
df2 <- rename(df2, intent = Intent)
df2 <- rename(df2, freedom_score = freedomscore)
df2 <- rename(df2, corruption_score = corruptionscore)
df2 <- rename(df2, elderly_population = elderlypop)
df2 <- rename(df2, healthcare_expenditure = healthcare)
df2 <- rename(df2, government_transparency = govtrans)
df2 <- rename(df2, government_efficiency = goveff)
df2 <- rename(df2, spending_efficiency = spendeff)
df2 <- rename(df2, year_month_frequency = year_month_freq)
# Method 1: jmv
jmv::logRegBin(
  data = df2,
  dep = intent,
  covs = vars(polity, freedom_score, corruption_score, population, elderly_population, healthcare_expenditure, government_transparency, government_efficiency, spending_efficiency, year_month_frequency),
  blocks = list(
    list(
      "polity",
      "freedom_score",
      "corruption_score",
      "population",
      "elderly_population",
      "healthcare_expenditure",
      "government_transparency",
      "government_efficiency",
      "spending_efficiency",
      "year_month_frequency")),
  refLevels = list(
    list(
      var="intent",
      ref="Developmental")))
# Method 2: Glm
df2$intent <- factor(df2$intent, exclude = c("", NA))
res <- glm(intent ~ polity + freedom_score + corruption_score + population 
           + healthcare_expenditure + elderly_population + government_transparency
           + government_efficiency + spending_efficiency + year_month_frequency,
           family = binomial,
           data = df2)
summary(res)
"Model coefficients and confidence intervals"
cbind(coef(res), confint(res, level = 0.95, method = "Wald"))

"Odds/Risk ratios and confidence intervals"
exp(cbind(OR = coef(res), confint(res, level = 0.95)))

car::infIndexPlot(res)

"Chan, G. and StatsNotebook Team (2020). StatsNotebook. (Version 0.1.2) [Computer Software]. Retrieved from https://www.statsnotebook.io"
"R Core Team (2020). The R Project for Statistical Computing. [Computer software]. Retrieved from https://r-project.org"

