# Deusto Business School / Machine Learning (FAB) - Universidad de Deusto
# Logistic regression

######################################################################
# Part 00: Cleaning space / Checking directory
######################################################################


rm(list = ls());cat("\014");graphics.off()
getwd()
dir()


######################################################################
# Part 01: Reading data
######################################################################


churn_data = read.csv("./churn.csv")
dim(churn_data)
str(churn_data)
summary(churn_data)

# Change variable types "Area.Code" and labels for "Churn"
churn_data$Area.Code <- as.factor(churn_data$Area.Code)
churn_data$State     <- as.factor(churn_data$State)
churn_data$Churn     <- as.factor(churn_data$Churn)
churn_data$Int.l.Plan <- as.factor(churn_data$Int.l.Plan)
churn_data$VMail.Plan <- as.factor(churn_data$VMail.Plan)

# Create dichotomous variable: 0 for Stay and 1 for Churn
churn_data$Churn_B <- ifelse(churn_data$Churn =="Churn",1,0)

# Changing Churn labels: FALSE as Stay and TRUE as Churn
levels(churn_data$Churn) = c("Stay", "Churn")

# Compare dataframe with previous result
summary(churn_data)

# Remove phone number from dataframe
churn_data$Phone <- NULL

saveRDS(churn_data, file="churn_data.rds")


##### SPLIT DATA ########################################
##### METHOD 1 ##########################################

## 75% of the sample size
smp_size <- floor(0.70 * nrow(churn_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(churn_data)), size = smp_size)

train1 <- churn_data[train_ind, ]
test1 <- churn_data[-train_ind, ]

##### SPLIT DATA ########################################
##### METHOD 2 ##########################################
set.seed(123)
sample <- sample(c(TRUE, FALSE), size=nrow(churn_data), replace=TRUE, prob=c(0.7,0.3))
train2 <- churn_data[sample, ]
test2 <- churn_data[!sample, ]


summary(train1)
summary(test1)

summary(train2)
summary(test2)

######################################################################
# Part 02: Logistic regression / Training Sample
######################################################################

# Look carefully: how to define a statistical model in R

# Our first logistic regression model: Day.Calls as explanatory variable of Churn

glm00.fits=glm(Churn~1,data=train1,family=binomial(link="logit"))
summary(glm00.fits)


glm01.fits=glm(Churn~CustServ.Calls,data=train1,family=binomial(link="logit"))
summary(glm01.fits)

glm02.fits=glm(Churn~CustServ.Calls+Day.Calls,data=train1,family=binomial(link="logit"))
summary(glm02.fits)

glm03.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan,data=train1,family=binomial(link="logit"))
summary(glm03.fits)

glm04.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State,data=train1,family=binomial(link="logit"))
summary(glm04.fits)

glm05.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State+VMail.Plan,data=train1,family=binomial(link="logit"))
summary(glm05.fits)

glm06.fits=glm(Churn~Day.Calls+CustServ.Calls+Int.l.Plan+State+VMail.Plan+Day.Mins,data=train1,family=binomial(link="logit"))
summary(glm06.fits)

# Nested models
# Check the decrease in the Residual Deviance column
# The most powerful explanatory variables achieve the greatest decrease in residual deviance
# Residual deviance => (observed values - fitted values)

deviance_table <- anova(glm00.fits, glm01.fits, glm02.fits, glm03.fits, glm04.fits, glm05.fits, glm06.fits, test="Chisq")
deviance_table

# This is how the Pr(>Chi) may be calculated too for each nested model
p2= 1-pchisq(deviance_table[2,4], df=deviance_table[2,3])
p3= 1-pchisq(deviance_table[3,4], df=deviance_table[3,3])
p4= 1-pchisq(deviance_table[4,4], df=deviance_table[4,3])
p5= 1-pchisq(deviance_table[5,4], df=deviance_table[5,3])
p6= 1-pchisq(deviance_table[6,4], df=deviance_table[6,3])
p7= 1-pchisq(deviance_table[7,4], df=deviance_table[7,3])

final_model <- glm(Churn~CustServ.Calls+Int.l.Plan+State+VMail.Plan+Day.Mins,data=train1,family=binomial(link="logit"))
summary(final_model)
str(final_model)
coeff[c("(Intercept)","Day.Mins")]
str(coeff)
coeff <- final_model[[1]]
residuals <- final_model[[2]]
summary(residuals)
str(final_model$fitted.values)
fitted <- final_model$fitted.values
mean(fitted)
final_model$deviance
######################################################################
# Part 03: Logistic regression / Test Sample
######################################################################

pred <- predict(final_model, newdata = test1, type = "response")
test1$Churn_pred <- ifelse(pred<0.5,"Stay","Churn")
tab <- with(test1,table(Churn,Churn_pred))
tab[,c("Stay","Churn")]
tab[1,1]
tab[1,2]
str(tab)
prop.table(table(test$Churn,test$Churn_pred))
summary(tab)
