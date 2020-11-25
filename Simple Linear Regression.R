# Load the data
# Salary_Data.csv 
data <- read.csv(file.choose(), header = T)
View(data)
attach(data)

# Exploratory data analysis
summary(data)
# years might not have outliers, salary might have outliers

library(Hmisc)

#for better understanding of data before visualization
describe(data)

# dotplot is part of lattice package
library("lattice") 

#create a temp directory if visualization does not work
#dir.create(tempdir())

# Graphical exploration
dotplot(YearsExperience   , main = "Dot Plot of Years of Experience ")
# x
# probably 2 clusters

dotplot(Salary , main = "Dot Plot of Salary")
# y
# 2 peaks/ clusters

boxplot(YearsExperience  , col = "red", horizontal = T)
# x
# no outliers, might have +ve skew

boxplot(Salary , col = "red", horizontal = T)
# y
# no outliers, might have normal skew

hist(YearsExperience )
# x
# 1 peak

hist(Salary)
# y
# 1 peak

# Normal QQ plot
qqnorm(YearsExperience )
qqline(YearsExperience ) # line from Q1 to Q3, if maximum points are on/ along the line then its normally distributed
# x
# might be normally distributed

qqnorm(Salary)
qqline(Salary)
# y
# might not be normally distributed

hist(YearsExperience  , prob = TRUE)# prob=TRUE for probabilities not counts
lines(density(YearsExperience ))# add a density estimate with defaults
lines(density(YearsExperience , adjust = 2), lty = "dotted")# add another "smoother" density
# x
# 2 curve

hist(Salary , prob = TRUE)          
lines(density(Salary))             
lines(density(Salary, adjust = 2), lty = "dotted")   
# y
# 2 curve

# Bivariate analysis
# Scatter plot
#x axis > input, y axis > output
plot(YearsExperience  , Salary , main = "Scatter Plot", col = "red", 
     col.main = "blue4", col.lab = "brown", xlab = "Years of Experience ", 
     ylab = "Salary", pch = 20)
# pch = 20 > circle, 17 > triangles
# direction = +ve, strength = moderate, linearity = non linear

# Correlation Coefficient will standardize the data and give value from -1 to +1
cor(YearsExperience , Salary)
# direction = +ve, strength = moderate (<0.85)

# Linear Regression model
reg <- lm(YearsExperience   ~ Salary , data = data) # Y ~ X
summary(reg)
# Residuals = 30 predicted values
# Intercept = beta 0, x  > beta 1 = Estimate; alpha = 0.5 by default
# H0 > beta 0 = 0, H1 > beta 0 != 0
# 6.3e-08 < 0.05 p low null go
# we have less than 5% chance of going wrong
# H0 > beta 1 = 0, H1 > beta 1 != 0
# 2e-16 < 0.05 p low null go
# r2 = 0.95 (if r2 > 0.80 than it is a strong model)
# p-value: = 2.2e-16 < 0.05 (overall significance of model is good)

# confidence interval
confint(reg, level = 0.95)
pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)
View(pred)

# ggplot for adding Regression line for data
library(ggplot2)
ggplot(data = data, aes(YearsExperience  , Salary ) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Evaluation the model for fitness 
cor(pred$fit, data$Salary )
reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse
# Linear model, r squared = 0.95, rmse = 0.57, problem = heteroscedacity
  
# Transformation Techniques to make a linear model
# input = log(x); output = y
plot(YearsExperience, Salary )
plot(log(YearsExperience), Salary )
# decrease

cor(log(YearsExperience), Salary )
# decreased

reg_log <- lm(Salary  ~ log(YearsExperience  ), data = data)
summary(reg_log)
# Coefficients = significant, r squared = 0.85

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")
pred <- as.data.frame(pred)
cor(pred$fit, data$Salary )
rmse <- sqrt(mean(reg_log$residuals^2))
rmse
# model = log, r squared = 0.85, rmse = 10302.9, problem = heteroscedacity, transformation = log

# Regression line for data
ggplot(data = data, aes(log(YearsExperience  ), Salary ) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Log transformation applied on 'y', exponential transformation
# input = x; output = log(y)
plot(YearsExperience  , Salary )
plot(YearsExperience  , log(Salary ))
# polynomial with 2 degrees

cor(YearsExperience  , log(Salary ))
# increment from previous model, but decreased from linear model

reg_log1 <- lm(log(Salary ) ~ YearsExperience  , data = data)
summary(reg_log1)
# Coefficients = significant, r squared = 0.93

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))
pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, data$Salary )
res_log1 = Salary  - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse
# exponential model, r squared = 0.93, rmse = 7213.23, problem = non linear, transformation = exponential

# Regression line for data
ggplot(data = data, aes(YearsExperience  , log(Salary )) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = data, aes(x = YearsExperience  , y = log(Salary ))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = data, aes(x = YearsExperience  , y = predlog$fit))

# Non-linear models = Polynomial models
# input = x & x^2 & x^3 (3-degree) and output = log(y)
# for calories data > + I(x^3)
reg2 <- lm(log(Salary) ~ YearsExperience   + I(YearsExperience  * YearsExperience), data = data)
summary(reg2)
# Coefficients = significant, r squared = increased 0.94

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)
pred <- as.data.frame(pred)
cor(pred$fit, data$Salary )
res2 = Salary  - pred$fit
rmse <- sqrt(mean(res2^2))
rmse
# polynomial with 2 degrees model, r squared = 0.94, rmse = 5391.082, transformation = polynomial

# Regression line for data
ggplot(data = data, aes(YearsExperience  , log(Salary )) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Data Partition
# Random Sampling
n <- nrow(data)#counting no of rows and storing in n
n1 <- n * 0.8 # 80% of n in n1
n2 <- n - n1
#sample() used for random sampling
train_ind <- sample(1:n, n1)
train <- data[train_ind, ]
test <-  data[-train_ind, ]

plot(train$YearsExperience  , log(train$Salary ))
plot(test$YearsExperience  , log(test$Salary ))

# y ~ x
model <- lm(Salary  ~ YearsExperience , data = train)
summary(model)
# Coefficients = significant, r squared = high

confint(model,level=0.95)
log_res <- predict(model,interval = "confidence", newdata = test)
predict_original <- exp(log_res) 

# converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Salary  - log_res

# calculate error/residual
test_error
test_rmse <- sqrt(mean(test_error^2))
test_rmse
# error increased because of split

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Salary  - log_res_train # calculate error/residual
train_error
train_rmse <- sqrt(mean(train_error^2))
train_rmse
# training error = 5936.159, testing error = 7240.513