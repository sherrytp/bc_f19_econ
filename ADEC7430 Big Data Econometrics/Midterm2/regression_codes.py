#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  8 02:06:13 2019
Created on Thu Feb 21 23:47:48 2019

@author: SPT 
Changed on Tue FEB 26 to regression_code.

#%% Import QuantEcon for future purpose
git clone https://github.com/QuantEcon/QuantEcon.py
python setup.py install

import quantecon as qe
print(qe.__version__)
from quantecon.markov import DiscreteDP
"""
# Load packages and read data
import numpy as np
np.set_printoptions(precision=4, suppress=True)
import pandas as pd
pd.set_option("display.width", 100)
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.graphics.api import interaction_plot, abline_plot
from statsmodels.compat import urlopen
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.api import anova_lm
from sklearn import datasets, linear_model, metrics
from sklearn.model_selection import train_test_split

def estimate_coef(x, y): 
	# number of observations/points 
	n = np.size(x) 

	# mean of x and y vector 
	m_x, m_y = np.mean(x), np.mean(y) 

	# calculating cross-deviation and deviation about x 
	SS_xy = np.sum(y*x) - n*m_y*m_x 
	SS_xx = np.sum(x*x) - n*m_x*m_x 

	# calculating regression coefficients 
	b_1 = SS_xy / SS_xx 
	b_0 = m_y - b_1*m_x 

	return(b_0, b_1) 

def plot_regression_line(x, y, b): 
	# plotting the actual points as scatter plot 
	plt.scatter(x, y, color = "m", 
			marker = "o", s = 30) 

	# predicted response vector 
	y_pred = b[0] + b[1]*x 

	# plotting the regression line 
	plt.plot(x, y_pred, color = "g") 

	# putting labels 
	plt.xlabel('x') 
	plt.ylabel('y') 

	# function to show plot 
	plt.show() 

def main(): 
	# observations 
	x = np.array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) 
	y = np.array([1, 3, 2, 5, 7, 8, 8, 9, 10, 12]) 

	# estimating coefficients 
	b = estimate_coef(x, y) 
	print("Estimated coefficients:\nb_0 = {} \nb_1 = {}".format(b[0], b[1])) 

	# plotting regression line 
	plot_regression_line(x, y, b) 

if __name__ == "__main__": 
	main() 


# load the boston dataset 
boston = datasets.load_boston(return_X_y=False) 

# defining feature matrix(X) and response vector(y) 
X = boston.data 
y = boston.target 

# splitting X and y into training and testing sets 
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.4,
													random_state=1) 

# create linear regression object 
reg = linear_model.LinearRegression() 

# train the model using the training sets 
reg.fit(X_train, y_train) 

# regression coefficients 
print('Coefficients: \n', reg.coef_) 

# variance score: 1 means perfect prediction 
print('Variance score: {}'.format(reg.score(X_test, y_test))) 

# plot for residual error 

## setting plot style 
plt.style.use('fivethirtyeight') 

## plotting residual errors in training data 
plt.scatter(reg.predict(X_train), reg.predict(X_train) - y_train, 
			color = "green", s = 10, label = 'Train data') 

## plotting residual errors in test data 
plt.scatter(reg.predict(X_test), reg.predict(X_test) - y_test, 
			color = "blue", s = 10, label = 'Test data') 

## plotting line for zero residual error 
plt.hlines(y = 0, xmin = 0, xmax = 50, linewidth = 2) 

## plotting legend 
plt.legend(loc = 'upper right') 

## plot title 
plt.title("Residual errors") 

## function to show plot 
plt.show() 

# Homoscedasticity: Homoscedasticity describes a situation in which the error term (that is, the “noise” or random disturbance in the relationship between the independent variables and the dependent variable) is the same across all values of the independent variables. As shown below, figure 1 has homoscedasticity while figure 2 has heteroscedasticity.


#%% Import and select the data
df4 = pd.read_stata('https://github.com/QuantEcon/QuantEcon.lectures.code/raw/master/ols/maketable4.dta')
df4 = df4[df4['baseco'] == 1]

# Add a constant variable
df4['const'] = 1

# Fit the first stage regression and print summary
results_fs = sm.OLS(df4['avexpr'],
                    df4[['const', 'logem4']],
                    missing='drop').fit()
print(results_fs.summary())

#%% %matplotlib inline
# Exercise on ANOVA Regression
# DATE: FEB 21 23:47:48 2019

url = 'http://stats191.stanford.edu/data/salary.table'
salary_table = pd.read_table(url)

#%% take a look at the database and run a simple regression 
plt.figure(figsize=(6,6))
symbols = ['D', '^']
colors = ['r', 'g', 'blue']
factor_groups = salary_table.groupby(['E','M'])
for values, group in factor_groups:
	i,j = values
	plt.scatter(group['X'], group['S'], marker=symbols[j], color=colors[i-1], s=144)

formula = 'S ~ C(E) + C(M) + X'
lm = ols(formula, salary_table).fit()
print(lm.summary())
"""
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      S   R-squared:                       0.957
Model:                            OLS   Adj. R-squared:                  0.953
Method:                 Least Squares   F-statistic:                     226.8
Date:                Thu, 14 Feb 2019   Prob (F-statistic):           2.23e-27
Time:                        19:45:58   Log-Likelihood:                -381.63
No. Observations:                  46   AIC:                             773.3
Df Residuals:                      41   BIC:                             782.4
Df Model:                           4                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
Intercept   8035.5976    386.689     20.781      0.000    7254.663    8816.532
C(E)[T.2]   3144.0352    361.968      8.686      0.000    2413.025    3875.045
C(E)[T.3]   2996.2103    411.753      7.277      0.000    2164.659    3827.762
C(M)[T.1]   6883.5310    313.919     21.928      0.000    6249.559    7517.503
X            546.1840     30.519     17.896      0.000     484.549     607.819
==============================================================================
Omnibus:                        2.293   Durbin-Watson:                   2.237
Prob(Omnibus):                  0.318   Jarque-Bera (JB):                1.362
Skew:                          -0.077   Prob(JB):                        0.506
Kurtosis:                       2.171   Cond. No.                         33.5
==============================================================================

Warnings:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
"""
#%% Have a look at the created design matrix 

lm.model.exog[:5]
lm.model.data.orig_exog[:5]
lm.model.data.frame[:5]

infl = lm.get_influence()
print(infl.summary_table())

df_infl = infl.summary_frame()

#%% Plot residuals 
resid = lm.resid
plt.figure(figsize=(6,6));
for values, group in factor_groups:
	i,j = values
	group_num = i*2 + j - 1  # for plotting purposes
	# x = [group_num] * len(group)
	# plt.scatter(x, resid[group.index], marker=symbols[j], color=colors[i-1], s=144, edgecolors='black')
plt.xlabel('Group');
plt.ylabel('Residuals');      # important to keep the ;

interX_lm = ols("S ~ C(E) * X + C(M)", salary_table).fit()
print(interX_lm.summary())

#%% difference to run an ANOVA regression
table1 = anova_lm(lm, interX_lm)
print(table1)

interM_lm = ols("S ~ X + C(E)*C(M)", data=salary_table).fit()
print(interM_lm.summary())

table2 = anova_lm(lm, interM_lm)
print(table2)

#%% Compare models and look at outliers 
drop_idx = abs(resid).argmax()
print(drop_idx)  # zero-based index
idx = salary_table.index.drop(drop_idx)

lm32 = ols('S ~ C(E) + X + C(M)', data=salary_table, subset=idx).fit()

print(lm32.summary())
print('\n')

interX_lm32 = ols('S ~ C(E) * X + C(M)', data=salary_table, subset=idx).fit()

print(interX_lm32.summary())
print('\n')


table3 = anova_lm(lm32, interX_lm32)
print(table3)
print('\n')


interM_lm32 = ols('S ~ X + C(E) * C(M)', data=salary_table, subset=idx).fit()

table4 = anova_lm(lm32, interM_lm32)
print(table4)
print('\n')

#%% Replot residuals 
try:
	resid = interM_lm32.get_influence().summary_frame()['standard_resid']
except:
	resid = interM_lm32.get_influence().summary_frame()['standard_resid']

plt.figure(figsize=(6,6))
for values, group in factor_groups:
    i,j = values
    idx = group.index
    plt.scatter(X[idx], resid[idx], marker=symbols[j], color=colors[i-1],
            s=144, edgecolors='black')
plt.xlabel('X[~[32]]');
plt.ylabel('standardized resids');

#%% Plot fitted values 
lm_final = ols('S ~ X + C(E)*C(M)', data = salary_table.drop([drop_idx])).fit()
mf = lm_final.model.data.orig_exog
lstyle = ['-','--']

plt.figure(figsize=(6,6))
for values, group in factor_groups:
    i,j = values
    idx = group.index
    plt.scatter(X[idx], S[idx], marker=symbols[j], color=colors[i-1],
                s=144, edgecolors='black')
    # drop NA because there is no idx 32 in the final model
    plt.plot(mf.X[idx].dropna(), lm_final.fittedvalues[idx].dropna(),
            ls=lstyle[j], color=colors[i-1])
plt.xlabel('Experience');
plt.ylabel('Salary');
