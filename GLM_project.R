library(tidyverse)
library(tidyr)
library(ggplot2)
library(sandwich)
library(msm)
library(dplyr)

week <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
index <- c(1.7, 1.9, 1.1, 0.8, 0.9, 1.3, 1.8, 1.5, 2.1, 0.7, 0.5)
total <- c(28, 29, 28, 27, 28, 25, 22, 23, 18, 19, 12)

df <- data.frame(week, index, total)
print(df)

summary(df)

#EXPLORE DATA
#plot and talk about trends

#plot week against total
plot(data = df, week, total, xlab = "Week", ylab = "Total number of students attending", 
     pch = 19, col = "blue")
cor.test(week, total) #-0.92

#plot score against total
plot(index, total, xlab = "Students performance on average", 
     ylab = "Total number of students attending", pch = 19, col = "blue")
cor.test(index, total) #0.25

plot(index, week, xlab = "Index", 
     ylab = "Week", pch = 19, col = "blue")
cor.test(index, week) #-0.31

var(total)
var(index)
#index mean 1.3, variance = 0.29
#total, mean 23.55, variance = 29.07

boxplot(df, col = "blue")

with(df,plot(week, total, xlab = "Week", ylab = "Total number of students attending", 
                 pch = 19, col = "blue") + lines(loess.smooth(week, total)))

#FIT SUITABLE MODEL
#Poisson distribution is appropriate as the data is not proportional 
#and is count data, binomial is not appropriate

#With a Poisson Distribution model we’re trying to figure out how some predictor variables affect a response variable
#the response variable is total
#the predictor variables are week and index

#Null model
poisson.null <- glm(total ~ 1, family=poisson(link="log"), data=df)
summary(poisson.null)
sum(resid(poisson.null, "pearson")^2)

#model 1
poisson1 <- glm(total ~ week, family=poisson(link="log"), data=df)
summary(poisson1)
sum(resid(poisson1, "pearson")^2)

plot(week, total, pch=19,col="blue", ylim = c(10,35),xlab = "Week", ylab = "Total number of students attending") 
points(week,poisson1$fitted,pch=19,col="red")

legend( "topright", c("Observed", "Fitted"), text.col=c("blue", "red"))
title("Linear model - week")


#COMPARE AND CONTRAST 2 MODELS. DEVIANCE ANALYSIS AND pseudo-R2 ON FINAL MODEL
#index as a covariate

poisson4 <- glm(total ~ index, family=poisson(link="log"), data=df)
summary(poisson4)

nullaic <- 1-(0.5*poisson.null$aic)
poisson2aic <- 3-(0.5*poisson2$aic)
(nullaic - poisson2aic)/nullaic

#model 2
poisson2 <- glm(total ~ week + index, family=poisson(link="log"), data=df)
summary(poisson2)
plot(week, total, pch=19,col="blue", ylim = c(10,35),xlab = "Week", ylab = "Total number of students attending") 
points(week,poisson2$fitted,pch=19,col="red")
legend( "topright", c("Observed", "Fitted"), text.col=c("blue", "red"))
title("Linear model - week + index")
sum(resid(poisson2, "pearson")^2)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(poisson2)

#model 3
poisson3 <- glm(total ~ I(week^2) + index, family=poisson(link="log"), data=df)
summary(poisson3)
sum(resid(poisson3, "pearson")^2)
plot(week, total, pch=19,col="blue", ylim = c(10,35),xlab = "Week", ylab = "Total number of students attending") 
points(week,poisson3$fitted,pch=19,col="red")
legend( "topright", c("Observed", "Fitted"), text.col=c("blue", "red"))
title("Linear model - week^2 + index")

#comparing 4 and 2
anova(poisson2, poisson4, test="Chisq")
#comparing 3 and 2
anova(poisson2, poisson3, test="Chisq")


cov.poisson2 <- vcovHC(poisson2, type="HC0")
std.err <- sqrt(diag(cov.poisson2))
r.est <- cbind(Estimate= coef(poisson2), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson2)/std.err), lower.tail=FALSE),
               LL = coef(poisson2) - 1.96 * std.err,
               UL = coef(poisson2) + 1.96 * std.err)

r.est

#COMPARING 1 AND 2
anova(poisson1, poisson2, test="Chisq")

#PREDICT WEEK 12 TOTAL
newdata = data.frame(week = 12, index = 1.5)
predict(poisson2, newdata = newdata, type = "response")
#15.6

#PREDICT INDEX FOR WEEK 12, CALCULATE PEARSONS RESIDUAL FOR WEEK 12
res.pearson <- residuals(poisson2, type="pearson")

(8 - 15.6314)/sqrt(15.6314)

plot(poisson2$fitted,res.pearson,xlab = "Fitted values", ylab = "Pearsons residuals", col = "red", pch=19)
dev.off()