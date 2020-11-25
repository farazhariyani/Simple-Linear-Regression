## Importing necessary packages
import numpy as np  
import pandas as pd

# Salary_Data.csv 
data = pd.read_csv("Salary_Data.csv")

# Exploratory data analysis:
data.describe()
data.columns = "YearsExp", "Salary" # changing column names for easy access.
data.shape

#Graphidata Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

# YearsExp
data.YearsExp.plot.bar()#bar plot
plt.hist(data.YearsExp) #histogram
plt.boxplot(data.YearsExp) #boxplot

# Salary
data.Salary.plot.bar()
plt.hist(data.Salary) #histogram
plt.boxplot(data.Salary) #boxplot

# Scatter plot
plt.scatter(x = data['Salary'], y = data['YearsExp'], color = 'green') 
# direction = +ve, strength = moderate, linearity = non linear

# correlation
np.corrcoef(data.Salary, data.YearsExp) 

import statsmodels.formula.api as smf
# Simple Linear Regression
model = smf.ols('YearsExp ~ Salary', data = data).fit()
model.params
model.summary()
# r squared = 0.95

pred1 = model.predict(pd.DataFrame(data['Salary']))
# Regression Line
plt.scatter(data.Salary, data.YearsExp)
plt.plot(data.Salary, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()
# Error dataculation
res1 = data.YearsExp - pred1
mse1 = np.mean(res1*res1)
rmse1 = np.sqrt(mse1)
rmse1
# 0.57

# Model building on Transformed Data
# Log Transformation
plt.scatter(x = np.log(data['Salary']), y = data['YearsExp'], color = 'brown')
np.corrcoef(np.log(data.Salary), data.YearsExp) #correlation
model2 = smf.ols('YearsExp ~ np.log(Salary)', data = data).fit()
model2.summary()
# r squared = 0.93

pred2 = model2.predict(pd.DataFrame(data['Salary']))
# Regression Line
plt.scatter(np.log(data.Salary), data.YearsExp)
plt.plot(np.log(data.Salary), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()
# Error dataculation
res2 = data.YearsExp - pred2
mse2 = np.mean(res2*res2)
rmse2 = np.sqrt(mse2)
rmse2
# 0.72

# Exponential transformation
# x = ; y = log()
plt.scatter(x = data['Salary'], y = np.log(data['YearsExp']), color = 'orange')
np.corrcoef(data.Salary, np.log(data.YearsExp)) #correlation
model3 = smf.ols('np.log(YearsExp) ~ Salary', data = data).fit()
model3.summary()
# r squared = 0.85

pred3 = model3.predict(pd.DataFrame(data['Salary']))
pred3_YearsExp = np.exp(pred3)
pred3_YearsExp
# Regression Line
plt.scatter(data.Salary, np.log(data.YearsExp))
plt.plot(data.Salary, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()
# Error dataculation
res3 = data.YearsExp - pred3_YearsExp
mse3 = np.mean(res3)
rmse3 = np.sqrt(mse3)
rmse3
# 0.04

# Polynomial transformation
# x = Salary; x^2 = Salary*Salary; y = log(YearsExp)
model4 = smf.ols('np.log(YearsExp) ~ Salary + I(Salary*Salary)', data = data).fit()
model4.summary()
# r squared = 0.90
pred4 = model4.predict(pd.DataFrame(data))
pred4_YearsExp = np.exp(pred4)
pred4_YearsExp

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree=2)
X = data.iloc[:, 1:2].values
X_poly = poly_reg.fit_transform(X)
# y = data.iloc[:, 0].values
plt.scatter(data.Salary, np.log(data.YearsExp))
plt.plot(X, pred4, color='red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error dataculation
res4 = data.YearsExp - pred4_YearsExp
mse4 = np.mean(res4)
rmse4 = np.sqrt(mse4)
rmse4
# 0.19

# Choose the best model using RMSE
data = {"MODEL":pd.Series(["SLR", "Log model", "Exp model", "Poly model"]), "RMSE":pd.Series([rmse1, rmse2, rmse3, rmse4])}
table_rmse=pd.DataFrame(data)
table_rmse

# The best model
# Exp
from sklearn.model_selection import train_test_split
train, test = train_test_split(data, test_size = 0.2)

finalmodel = smf.ols('np.log(YearsExp) ~ Salary', data = train).fit() 
finalmodel.summary()
# r squared = 0.84

# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
pred_test_YearsExp = np.exp(test_pred)
pred_test_YearsExp
# Model Evaluation on Test data
test_res = test.YearsExp - pred_test_YearsExp
test_mse = np.mean(test_res*test_res)
test_rmse = np.sqrt(test_mse)
test_rmse

# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
pred_train_YearsExp = np.exp(train_pred)
pred_train_YearsExp
# Model Evaluation on train data
train_res = train.YearsExp - pred_train_YearsExp
train_mse = np.mean(train_res*train_res)
train_rmse = np.sqrt(train_mse)
train_rmse
# train = 0.92, test = 1.50