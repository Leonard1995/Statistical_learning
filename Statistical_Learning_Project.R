library(car)
library(olsrr)
library(lmtest)
library(tseries)
library(caret)
setwd("/statistical learning/progetto")
data <- read.csv("wages.csv", header = TRUE)

#we change the value 0 in experience of 4 observation to make the logarithm possible
data[c(960,812,879,621),2] <- 1 


#Let's first have a look on the behavior of the linear model without changes
lm1 <- lm(wage ~ ., data=data)
summary(lm1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs per page
plot(lm1)
coefficients <- summary(lm1)$coefficients

#1) Influential values and outliers.
#performign outliers test
outlierTest(lm1, n.max = 30)
mean.exper <- data[data$exper==9 & data$educ==13,]
mean(mean.exper$wage) #looking for the mean of people with same attributes
#of case 603
ordered.wage <- sort.list(data[,1])
ordered.wage <- data[ordered.wage,1]
plot(ordered.wage) # plotting the shape of the ordered list of wages
#Since the variable wage behave exponentially, a possible transformation could be a logarithmic 
#transformation. 
data.no.outlier <- data[-603,]
lm.no.outlier <- lm(wage~., data=data.no.outlier)
summary(lm.no.outlier)


#2) Non-linearity non correlation between residuals and regressors


cor.test(data$exper, lm1$residuals) 
cor.test (data$educ, lm1$residuals)



white.test(data.no.outlier$exper,data.no.outlier$wage, type="F")
white.test(data.no.outlier$educ, data.no.outlier$wage,  type="F")

boxTidwell(wage~exper+educ, other.x=~as.factor(looks)+union+gender+marital+city+goodhlth+ethnicity+region, data = data.no.outlier)
educ275 <- data.no.outlier$educ^2.75

lm.no.outlier2 <- lm(wage~log(exper)+educ275+looks+union+gender+marital+city+goodhlth+ethnicity+region, data.no.outlier)
summary(lm.no.outlier2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm.no.outlier2)


#3) Heteroscedasticity

#recode varriable looks
data.no.outlier[data.no.outlier$looks == 3,3] <- "Average"
data.no.outlier[data.no.outlier$looks ==1,3] <- "Homely"
data.no.outlier[data.no.outlier$looks == 2,3] <- "Homely"
data.no.outlier[data.no.outlier$looks==  4,3] <- "Good Looking"
data.no.outlier[data.no.outlier$looks == 5,3] <- "Good Looking"

#changing baselies
data.no.outlier <- within(data.no.outlier, gender <- relevel(gender, ref = "male"))
data.no.outlier <- within(data.no.outlier, union <- relevel(union, ref = "yes"))
data.no.outlier <- within(data.no.outlier, ethnicity <- relevel(ethnicity, ref = "white"))

ols_test_f(lm.no.outlier2)
ols_test_bartlett(data.no.outlier, exper, educ, wage)
ols_test_score(lm.no.outlier2)
ols_test_breusch_pagan(lm.no.outlier2)

wageBCMod <- caret::BoxCoxTrans(data.no.outlier$wage)
print(wageBCMod)

#Box-Cox transformation suggests that variable wage should be transformed by the natural logarithm
#as we thought previously. Let us fit a new model with the logarithm of wage:


data.no.outlier <- cbind(data.no.outlier, wage_new=predict(wageBCMod, data.no.outlier$wage))
lmMod_bc <- lm(wage_new ~ log(exper)+educ275+looks+union+gender+marital+city+goodhlth+ethnicity+region, data.no.outlier)
summary(lmMod_bc)
layout(matrix(c(1,2,3,4),2,2))
plot(lmMod_bc)
coefficientsfinal <- summary(lmMod_bc)$coefficients

#4) Model with interactions
lm.interaction <- lm(wage_new ~ ethnicity:educ275+ethnicity:log(exper)+gender:looks+educ275*gender+log(exper)*gender+looks+union+marital+city+region, data.no.outlier) 
summary(lm.interaction)
co <- summary(lm.interaction)$coefficient

#5) Weighted least regression

lmWLS <- lm(wage_new ~ log(exper)+educ275+looks+union+gender+marital+city+goodhlth+ethnicity+region, data.no.outlier, weights = 1/data.no.outlier$wage) 
summary(lmWLS)
residuals <-  lmWLS$residuals
standardized_residuals = residuals/sqrt(data.no.outlier$wage)
variance1 = var(standardized_residuals[data.no.outlier$wage < median(data.no.outlier$wage)])
variance2 = var(standardized_residuals[data.no.outlier$wage > median(data.no.outlier$wage)])
variance1
variance2
layout(matrix(c(1,2,3,4),2,2))
plot(lmWLS)


