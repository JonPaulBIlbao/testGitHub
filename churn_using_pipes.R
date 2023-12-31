library(dplyr)


### LEARNING ABOUT PIPES

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



# FIRST WE DO IT WITHOUT PIPES

results_State <- group_by(datos,State)
datos_select <- select(results_State,State,CustServ.Calls,Account.Length,Churn_B)
summarized_cust <- summarise(datos_select,call=mean(CustServ.Calls), account_m=mean(Account.Length), 
                             Churn_B=mean(Churn_B))
final_set <-filter(summarized_cust , Churn_B>0.15)



### NOW WE DO IT WITH PIPES AND THE FINAL DATAFRAME IS THE SAME as final_set

final_set_B <-
datos %>% 
  group_by(State) %>% 
  select(State,CustServ.Calls,Account.Length,Churn_B) %>% 
  summarise(call=mean(CustServ.Calls), account_m=mean(Account.Length), Churn_B=mean(Churn_B)) %>% 
  filter(Churn_B>0.15 )
