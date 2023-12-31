# Deusto Business School / Statistics II (FAB) - Universidad de Deusto
# Logistic regression

######################################################################
# Part 00: Checking and installing required packages
######################################################################


packages = c("ggplot2","psych","vcd","corrplot","GGally","reshape2",
             "ggraph","igraph","dplyr","datasauRus","ggforce","qgraph")
newpack  = packages[!(packages %in% installed.packages()[,"Package"])]

if(length(newpack)) install.packages(newpack)
a=lapply(packages, library, character.only=TRUE)

rm(list = ls());cat("\014");graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()




######################################################################
# Part 01: Reading data
######################################################################
rm(list = ls());cat("\014")

datos = read.csv("./churn.csv")
dim(datos)
str(datos)


# Change variable type "Area.Code" and labels for "Churn"
datos$Area.Code = factor(datos$Area.Code)
levels(datos$Churn) = c("Stay", "Churn")
summary(datos)

# Cancel phone number
datos$Phone = NULL

datos$Churn_B <- ifelse(datos$Churn =="True",1,0)

######################################################################
# Part 02: Exploratory analysis
######################################################################

summary(datos)

# Exploring: Churn vs vMail.Plan
# ==========
table_A <- table(datos$Churn , datos$VMail.Plan)
table_A
prop.table(table_A,1)
prop.table(table_A,2)


# Remember chi-square test of independence?
# Null hypothesis: both variables ARE independent -> No association
# Check the p-value (alpha error) and remember:
#           <0.05 : we may reject the null hypothesis
#           >0,05 : we should not reject the null hypothesis

chisq.test(datos$Churn , datos$VMail.Plan)
# So your conclusion in terms of business is ...

# Exploring: Churn vs Int.l.Plan
# ==========
table(datos$Churn , datos$Int.l.Plan)
# Follow the bivariate statistical analysis and


# Exploring: Churn vs Customer Service Calls
# ==========

table_C <- table(datos$CustServ.Calls, datos$Churn)
table_C

addmargins(table_C) # Total is added


#### ROW PERCENTAGES
prop.table(table_C,1) 
round(100*prop.table(table_C,1), digits=2)

#### COLUMN PERCENTAGES
prop.table(table_C,2)
round(100*prop.table(table_C,2), digits=2)


chisq.test(datos$CustServ.Calls, datos$Churn)
# Something is not working properly

# What do you know about boxplots?


boxplot(datos$CustServ.Calls ~ datos$Churn, horizontal=TRUE,
        ylab="Churn", xlab="Customer Service Calls", las=1,
        main="Exploratory analysis with boxplots")

# Instead of the chi-square test, a t-test is performed... why?

# t.test()
# ==========
t.test(CustServ.Calls ~ Churn, data=datos)


# Exploring: TRY YOURSELF Churn vs Account.Length
# ==========


# ADVANCED VISUAL TOOLS
# ==========

library(GGally)
ggpairs(datos[,c(2:7,20)])
ggpairs(datos[,c(2,7,17,19,20)])

# Colour based on Churn
ggpairs(datos[,c(2:7,20)], aes(col = Churn))

######################################################################
# Part 03: Logistic regression
######################################################################

# Look carefully: how to define a statistical model in R

# Our first logistic regression model: Day.Calls as explanatory variable of Churn

glm01.fits=glm(Churn~Day.Calls,data=datos,family=binomial(link="logit"))
summary(glm01.fits)

glm02.fits=glm(Churn~Day.Calls+CustServ.Calls,data=datos,family=binomial(link="logit"))
summary(glm02.fits)

glm03.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan,data=datos,family=binomial(link="logit"))
summary(glm03.fits)

glm04.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State,data=datos,family=binomial(link="logit"))
summary(glm04.fits)

glm05.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State+VMail.Plan,data=datos,family=binomial(link="logit"))
summary(glm05.fits)

glm06.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State+VMail.Plan+Day.Mins,data=datos,family=binomial(link="logit"))
summary(glm06.fits)

# Nested models
# Check the decrease in the Residual Deviance column
# The most powerful explanatory variables achieve the greatest decrease in residual deviance
# Residual deviance => (observed values - fitted values)

anova(glm01.fits, glm02.fits, glm03.fits, glm04.fits, glm05.fits, glm06.fits)

p2= 1-pchisq(131.601, df=1)
p3= 1-pchisq(189.064, df=1)
p4= 1-pchisq(87.764, df=50)
p5= 1-pchisq(43.551, df=1)


# Look closer ---> second model

coef(glm02.fits)
summary(glm02.fits)$coef
summary(glm02.fits)$coef[,4]
glm02.probs=predict(glm02.fits,type="response")
glm02.probs

datos$pred <- glm02.probs

# Boxplot with predicted probabilities
# ==========
boxplot(datos$pred ~ datos$Churn, horizontal=TRUE,
        ylab="Churn", xlab="Churn Probability prediction", las=1,
        main="Prediction based on logistic model")

