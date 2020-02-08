## Project Part I & II
rawdata <- read.table("C://Users//Connor//Desktop//CDI.txt")
colnames(rawdata)
mydata <- rawdata[,c("V5", "V8", "V9", "V12", "V15", "V16", "V17")]
colnames(mydata) <- c("population", "beds", "physicians", "degree", "capita_in", "income", "region")
head(mydata)

predictors <- c("population", "beds", "physicians", "degree", "capita_in", "income", "region")
i = 1




## Project 1.43 a)
model1_1 <- lm(physicians ~ population, data = mydata)
model1_2 <- lm(physicians ~ beds, data = mydata)
model1_3 <- lm(physicians ~ income, data = mydata)

sm1 = summary(model1_1)
sm2 = summary(model1_2)
sm3 = summary(model1_3)

sm1$coefficients
sm2$coefficients
sm3$coefficients

## estimated function (physicians ~ population): Y = 0.0035X + 78.1231
## estimated function (physicians ~ beds): Y = 1.2157X + 257.5505
## estimated function (physicians ~ income): Y = 0.1603X + 197.4375


## Project 1.43 b)
Y <- mydata[,c("physicians")]
X <- mydata[,c("population")]
plot(X, Y, xlab = 'Total population', ylab = 'Number of active physicians', main = 'Physicians vs Population')
abline(model1_1, col = "red")

Y <- mydata[,c("physicians")]
X <- mydata[,c("beds")]
plot(X, Y, xlab = 'Number of hospital beds', ylab = 'Number of active physicians', main = 'Physicians vs Beds')
abline(model1_2, col = "red")

Y <- mydata[,c("physicians")]
X <- mydata[,c("income")]
plot(X, Y, xlab = 'Total personal income', ylab = 'Number of active physicians', main = 'Physicians vs Income')
abline(model1_3, col = "red")

## They three all appear to have good fit of linear regression relation.


## Project 1.43 c)
MSE1 = mean(sm1$residuals^2)
MSE2 = mean(sm2$residuals^2)
MSE3 = mean(sm3$residuals^2)

MSE1
MSE2
MSE3

## The numbers of hospital beds has the smallest variability around the fitted regression line




##Project 1.44 a)
region1 <- mydata[which(mydata$region=='1'), c(4,5)]
region2 <- mydata[which(mydata$region=='2'), c(4,5)]
region3 <- mydata[which(mydata$region=='3'), c(4,5)]
region4 <- mydata[which(mydata$region=='4'), c(4,5)]

sum1 = summary(lm(capita_in ~ degree, data = region1))
sum2 = summary(lm(capita_in ~ degree, data = region2))
sum3 = summary(lm(capita_in ~ degree, data = region3))
sum4 = summary(lm(capita_in ~ degree, data = region4))

sum1$coefficient
sum2$coefficient
sum3$coefficient
sum4$coefficient

## estimated function (region1): Y = 522.16X + 9223.82
## estimated function (region2): Y = 238.67X + 13581.41
## estimated function (region3): Y = 330.61X + 10529.79
## estimated function (region4): Y = 440.32X + 8615.05


## Project 1.44 b)

## The functions are similar for the four regions. (*Extension Needed)


## Project 1.44 c)
mse1 = mean(sum1$residuals^2)
mse2 = mean(sum2$residuals^2)
mse3 = mean(sum3$residuals^2)
mse4 = mean(sum4$residuals^2)

mse1
mse2
mse3
mse4

## MSE are approximately the same for three regions except for the geographic region 2, its MSE is smaller than the three other ones.


## Project 2.62
summary(model1_1)$r.square
summary(model1_2)$r.square
summary(model1_3)$r.square

## Base on r square result of three terms, the numbers of beds has the largest reduction in variability in the numbers of active physicians.

## start of 2.63 - Confidence Intervals for 4 regional regression lines
n1 = nrow(region1)
print(n1)

n2 = nrow(region2)

n3 = nrow(region3)

n4 = nrow(region4)

alpha = 0.10
## CI for Region 1
beta1.1hat = sum1$coefficients[[2]]
print(beta1.1hat)
se.beta1.1hat = sum1$coefficients[2,2]
print(se.beta1.1hat)

critical.value1 = qt(1-alpha/2, n1-2)
print(critical.value1)
lower.bound1 = beta1.1hat - critical.value1*se.beta1.1hat
upper.bound1 = beta1.1hat + critical.value1*se.beta1.1hat
CI.1 = c(lower.bound1, upper.bound1)
print(CI.1)

## CI for Region 2
beta1.2hat = sum2$coefficients[[2]]
print(beta1.2hat)
se.beta1.2hat = sum2$coefficients[2,2]
print(se.beta1.2hat)

critical.value2 = qt(1-alpha/2, n2-2)
print(critical.value2)
lower.bound2 = beta1.2hat - critical.value2*se.beta1.2hat
upper.bound2 = beta1.2hat + critical.value2*se.beta1.2hat
CI.2 = c(lower.bound2, upper.bound2)
print(CI.2)

## CI for Region 3
beta1.3hat = sum3$coefficients[[2]]
print(beta1.3hat)
se.beta1.3hat = sum3$coefficients[2,2]
print(se.beta1.3hat)

critical.value3 = qt(1-alpha/2, n3-2)
print(critical.value3)
lower.bound3 = beta1.3hat - critical.value3*se.beta1.3hat
upper.bound3 = beta1.3hat + critical.value3*se.beta1.3hat
CI.3 = c(lower.bound3, upper.bound3)
print(CI.3)

## CI for Region 4
beta1.4hat = sum4$coefficients[[2]]
print(beta1.4hat)
se.beta1.4hat = sum4$coefficients[2,2]
print(se.beta1.4hat)

critical.value4 = qt(1-alpha/2, n4-2)
print(critical.value4)
lower.bound4 = beta1.4hat - critical.value4*se.beta1.4hat
upper.bound4 = beta1.4hat + critical.value4*se.beta1.4hat
CI.4 = c(lower.bound4, upper.bound4)
print(CI.4)
## The Slopes seem to vary relatively far from each other for each regional regression line

## ANOVA Tables
fit.region1 = lm(capita_in ~ degree, data = region1)
fit.region2 = lm(capita_in ~ degree, data = region2)
fit.region3 = lm(capita_in ~ degree, data = region3)
fit.region4 = lm(capita_in ~ degree, data = region4)

anova(fit.region1)

anova(fit.region2)

anova(fit.region3)

anova(fit.region4)

## Results of F tests



## start of 3.25 - 3 residual plots against X
residuals_1 = sm1$residuals
plot(x=mydata$population, y = residuals_1, xlab = 'Total Population', ylab = 'Residuals for active physicians',
     main = 'Residual Plot For Total Population')
abline(h=0, col='red')

residuals_2 = sm2$residuals
plot(x=mydata$beds, y = residuals_2, xlab='Number of hospital beds', ylab = 'Residuals for active physicians',
     main = 'Residual Plot For Number of Hospital Beds')
abline(h=0, col= 'red')

residuals_3 = sm3$residuals
plot(x=mydata$income, y = residuals_3, xlab = 'Total personal income', ylab = 'Residuals for active physicians',
     main = ' Residual Plot For Total Personal Income')
abline(h=0, col = 'red')

## 3.25 normal probability plot
qqplot_1 = qqnorm(residuals_1, main = 'Normal Q-Q Plot (Total Population)')
qqline(residuals_1, col ='red')
cor(qqplot_1$x, qqplot_1$y) ## = 0.8890141

qqplot_2 = qqnorm(residuals_2, main = 'Normal Q-Q Plot (Hospital Beds)')
qqline(residuals_2, col = 'red')
cor(qqplot_2$x, qqplot$y) ## = 0.9042741

qqplot_3 = qqnorm(residuals_3, main = 'Normal Q-Q Plot (Income)')
qqline(residuals_3, col = 'red')
cor(qqplot_3$x, qqplot_3$y) ## = 0.8993738

## Is linear regression model .... ???





