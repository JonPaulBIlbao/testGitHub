# Create a histogram from 1000 random normal data
hist(rnorm(1000))
# So I can add more R lines here
# And check them in R
# I want to plot a normal distribution
# I want to plot a normal distribution
# Show the summary of car dataset
summary(cars)
# Load cars dataset in the environment
data(cars)
# Show the plot of cars dataset
plot(cars)
# Add regression line to the plot
abline(lm(dist ~ speed, data = cars))
# Calculate the correlation between speed and distance
cor(cars$speed, cars$dist)
# Trying changes in Posit Cloud
install.packages("gitcreds")
library(gitcreds)

gitcreds::gitcreds_set()
# Ante la pregunta: "Enter password or token"
# introducir el token copiado en el paso anterior