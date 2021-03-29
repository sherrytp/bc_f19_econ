# -*- coding: utf-8 -*-
"""
Created on Wed Jan 30 22:53:39 2019

@author: RV
"""

#%%
#%% Setup 
import os

projFld = "C:/Users/RV/Documents/Teaching/2019_01_Spring/ADEC7430_Spring2019/Lecture03"
codeFld = os.path.join(projFld, "PyCode")
fnsFld = os.path.join(codeFld, "_Functions")
outputFld = os.path.join(projFld, "Output")
rawDataFld = os.path.join(projFld, "RawData")
savedDataFld = os.path.join(projFld, "SavedData")

#%% Part 1: Sorting lists
templist = ['25', '010', '1', '10', '23']
templist.sort() # notice - this is in-place, we don't assign the operation result!
print(templist)

#%% Part 2: substrings of strings
# execute these statements one by one, figure out what they do
# search online only after you've tried for 2 minutes to figure out what they do!!!
tempstring = 'My first string'
print(tempstring[0])
print(tempstring[4])
print(tempstring[:10])
print(tempstring[1:5])
print(tempstring[1:5:2])
print(tempstring[1:5:3])
print(tempstring[1:5:4])

#%% Iterate over a list, do something
templist = [i for i in range(-10, 10, 2)] # read about range
print(templist)
print([2*x + 1 for x in templist])
print([str(x) for x in templist])

# the order of operation matters:
# sorting AFTER type conversion (from numeric to characters)
sorted([str(x) for x in templist])
# sorting BEFORE type conversion (from numeric to characters)
print([str(x) for x in sorted(templist)])

#%% Part 4: sort list by initial letter, lower cased
templist = ['pandas', 'numpy', 'DataFrame', 'boxplot', 'Seaborn']
templist.sort()
templist # do you like this order?

templist_lower = [x.lower() for x in templist]
print(templist_lower) # OK but this is a different list - changed
templist_lower.sort()
sorted(templist, key=lambda x: templist_lower.index(x.lower()))
# OK this is probably one of the most complex things we'll do with lists.


#%% Running an ANOVA
# Import rTrain exported in Lecture 02 - it should be in SavedData in Lecture02
# one-way ANOVA
import pandas as pd
import statsmodels as sm
from statsmodels.formula.api import ols
# from statsmodels.stats.anova import anova_lm

rTrain = pd.read_csv(os.path.join(rawDataFld, "Lecture2_train.csv"), sep=',', dtype=object)
rTrain.Age = pd.to_numeric(rTrain.Age)
rTrain.Pclass = pd.to_numeric(rTrain.Pclass)
rTrain.Survived = pd.to_numeric(rTrain.Survived)
test_lm = ols('Age ~ C(Pclass, Sum)*C(Survived, Sum)', data=rTrain).fit()
test_lm.summary()
table = sm.stats.anova.anova_lm(test_lm, typ=2) # Type 2 ANOVA DataFrame
# table = anova_lm(test_lm, typ=2) # Type 2 ANOVA DataFrame

# we can run ANOVA with scipy as well
import scipy
anova_scipy = scipy.stats.f_oneway(rTrain.loc[rTrain['Pclass'] == 1].Age, rTrain.loc[rTrain['Pclass'] == 2].Age)
print(anova_scipy)

#%% Does the price vary significantly by departure port?
# HW investigate distribution (one-way Anova) of Fare with respect to Embarked

#@@ ===== to replace code below with Python code ====
# ===== R Code for reference ====
#rTrain[,Embarked_f:=as.factor(Embarked)]
#anova1 <- oneway.test(Fare ~ Embarked_f, data=rTrain)
#anova1
#summary(anova1)

# better option - "aov"
#anova2 <- aov(Fare ~ Embarked, data = rTrain)
#anova2
#summary(anova2)
# ====== end of R Code ==========

#==============================================================================
# Multiple Linear Regression
#==============================================================================
# Assumptions... and what do they mean?
Rpdata = pd.read_csv(os.path.join(rawDataFld, "alr4_Rpdata.csv"))

import statsmodels.api as sm
import seaborn as sns
lmmodel = sm.OLS(Rpdata["y"], Rpdata[["x1", "x2", "x3", "x4", "x5", "x6"]]).fit()

# does this look like a good model (numerically speaking)?
lmmodel.summary()    

# how about we "dig deeper"? - residual plots
lmmodel_fitted_y = lmmodel.fittedvalues
lmmodel_residuals = lmmodel.resid
sns.residplot(lmmodel_fitted_y, 'y', data=Rpdata, lowess=True)
# Uh-oh! The residuals are not random! Or are they? What makes them non-random?
# Participate in the online discussion on randomness!
# HW follow up with comments on what is randomness, and how is this relevant to our analysis?
