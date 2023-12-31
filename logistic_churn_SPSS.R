# ==============================================================================
# LOGISTIC REGRESSION FROM
# https://datascienceplus.com/predict-customer-churn-logistic-regression-decision-tree-and-random-forest/
# ==============================================================================


# Eliminamos todo aquello que pueda quedar vivo en el entorno de trabajo y establecemos el directorio de
# trabajo al directorio donde está ubicado este script.

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <- read.csv('Telco_Customer_Churn.csv')
str(churn)

