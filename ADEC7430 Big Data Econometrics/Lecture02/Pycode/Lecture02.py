# -*- coding: utf-8 -*-
"""
Created on Thu Jan 24 20:03:24 2019

@author: RV
"""

# Python(R)
# Modeules/packageslibraries
# OS - submodules/path/join
    #eg. (os.path.join)
# pandas
# scipy
# onspy

#%% Setup
import os


# projFld = "C:/Users/RV/Documents/Teaching/2019_01_Spring/ADEC7430_Spring2019/Lecture02"
projFld = '/Users/sherrytp/Desktop/bc_f19_econ/ADEC7430 Big Data Econometrics/Lecture02'
codeFld = os.path.join(projFld, "PyCode")
fnsFld = os.path.join(codeFld, "_Functions")
outputFld = os.path.join(projFld, "Output")
rawDataFld = os.path.join(projFld, "RawData")
savedDataFld = os.path.join(projFld, "SavedData")

#%% load some functions
fnList = ["fn_logMyInfo"] # this is a list
for fn in fnList:
    exec(open(os.path.join(fnsFld, fn + ".py")).read())
# Explain: name, extension name, read+write
# create also a file where we will log some data
logf = os.path.join(rawDataFld, "logfile.csv")

# test writing to log
from _Functions.fn_logMyInfo import fn_logMyInfo
fn_logMyInfo("test writing to log", useConsole=True, useFile=logf)

# can we enhance what we write? How about we add a timestamp?

#%% introduction to datetime
import datetime as DT

# what time is it now?
DT.datetime.now() # do you like the format?
# micro-seconds

# can we format this more friendly?
nowtime = DT.datetime.now()
nowtimef = nowtime.strftime(format="%Y-%m-%d %H:%M:%S") # remember this
nowtimef 
type(nowtimef) # so this is a string

# let's add microseconds as well
nowtimef = nowtime.strftime(format="%Y-%m-%d %H:%M:%S.%f") # remember this
nowtimef

#%%
# do you want to keep writing the long formula above? I'd rather write a function
def nowdt():
    return(DT.datetime.now().strftime(format="%Y-%m-%d %H:%M:%S.%f"))
nowdt()

#%% now let's add timestamp to our log output
fn_logMyInfo(nowdt() + "," + " second test writing to log", useFile = logf)
# open the log file - do you notice anything unpleasant? e.g. the messages are appended
# can we try to save to a new line?
fn_logMyInfo("\n" + nowdt() + "," + " second test writing to log", useFile = logf)

# this is better... but lengthened our logging function quite a lot
#@@@@ add here a wrapper for this function, with defaults for: newline, timestamp, using given file
# Excel file doesn't show a micro-second digits but a text reader will do.
#%% Remember how this function works...
#@@@@ how to print the function so we can see how it's put together?


#%%
#==============================================================================
# Exploratory analysis
#==============================================================================

# Data from Kaggle's Titanic challenge comes already split in Train & Test
# See slides - why do we need this split?

# point to the files
rawTrainFile = os.path.join(rawDataFld, "Lecture2_train.csv")
rawTestFile  = os.path.join(rawDataFld, "Lecture2_test.csv")

#%% Pandas - transformation and data management, package
# read them into pandas DataFrame
import pandas as pd
rawTrain = pd.read_csv(rawTrainFile, sep=',')

# ParserWarning: Falling back to the "python" engine b/c the "c" engine doesn't support regex separators (seperators > 1 char and different from '\s+' are interpreted as regax)
# So you cannot do sep = ',,' and we don't have to do anything with lines '\n'
# If you want to test the warnings you would get when longer separators exist...


# let's understand a bit this DataFrame
# size
rawTrain.shape # how many rows, how many columns?

# print top 7 records
rawTrain.head(7) # notice the dots?

# let's expand the view - need options for pandas printout
pd.set_option('display.width', 1000)
pd.set_option('max_colwidth', 500)
pd.set_option('display.max_columns', 12)
rawTrain.head(7) # does it look better? did the dots vanish or are they still there?

# What is rawTrain?
type(rawTrain)

# list all columns
rawTrain.columns

# list all columns AND their types
rawTrain.dtypes # to think... if CSV is a text file, how did pandas figure out the types?
# does it make sense to have Age "float64"? (fractional) B/C Age has missing values
# int64 doesn't allow for missing values

#L Let's force pandas to read everything as a character
rawTrain_c = pd.read_csv(rawTrainFile, sep=',', dtype=object)
rawTrain_c.dtypes
rawTrain_c.head(5)

# for numeric variables, try this:
rawTrain.describe() # anything interesting here? See Age. 
# Only numeric data. Missing data in Age b/c less counts than others.

# are there missing values?
pd.isnull(rawTrain)
pd.isnull(rawTrain).astype(int)
pd.isnull(rawTrain).astype(int).aggregate(sum)

# can we see some records with missing Age valeus?
agecond = rawTrain.Age.isnull()
agecond
agecond.value_counts() # so here are our 177 missing recordss

rawTrain.loc[agecond].head(10).drop(columns=['Name', 'Ticket'])
# Data.location[which row is missing = True].first 10 rows and drop them and create a new dataframe
# how is missing age represented?  NaN

#%% maybe drop some columns/vars - e.g. Name
rTrain = rawTrain # so very dangerous with data.table, which is otherwise a very powerful tool

rTrain = rTrain.drop(columns=['Name'])

rTrain.shape
rawTrain.shape
# so changing the copy did not change the original - take a moment to enjoy this
# Which, changing copies in R will also change the original
#%% save a copy of the data for faster later access
rTrainFile = os.path.join(savedDataFld, "rTrain.pkl")
rTrain.to_pickle(rTrainFile)
rTrain.to_csv(rTrainFile+".csv")
# go open the last saved CSV file; what's with the first column?
rTrain.to_csv(rTrainFile + ".csv", index=None)
# go try again - did that column vanish?

#%% distribution of ages?

# simple boxplot
rTrain.boxplot(column=['Age'])
# more complex - Age boxplot by "Survived"
rTrain.groupby('Survived').boxplot(column=['Age'])
# more complex - Age boxplot by passenger class (Pclass)
rTrain.groupby('Pclass').boxplot(column=['Age'])
# HW: In 1-2 sentences describe what this boxplot tells you
rTrain.groupby('Survived').boxplot(column=['Pclass'])
# HW: In 1-2 sentences describe what this boxplot tells you

#%% Quantitles

rTrain.quantile(.1)
rTrain.quantile(.9)
rTrain.quantile([0.05*i for i in range(20)])
# Notice the choice of layout between one value and multiple-values for quantile request!

#%% Character functions - trim
# do we need to trim (?) some names or other character values?
# in Python this is called stripping the leading/trailing characters
' left and right '.strip()

#%% working with counts
# table(rTrain[,Age,by=list(Sex)]) - R version

# simple counts by value - in descending order of count/frequency
rTrain.Age.value_counts() # ughh, how many passengers of age 62 are there? Hard to see...
# same counts but now sort by actual values (index for the DataFrame)
rTrain.Age.value_counts().sort_index()

# counts/frequency by groups:
rTrain.groupby(['Sex']).Age.value_counts()
rTrain.groupby(['Sex']).Age.value_counts().sort_index()  # sorted by (descending) frenquency
# better sorting, but a bit cumbersome to read already... see the left-most index (gender)

# in such cases, resetting the (multi)index is the best
rTrain.groupby(['Sex']).Age.value_counts().sort_index().reset_index(name='freq')
# HW: from left to right, explain what the "chain" of the commands above does

#%% Create new variables - grouped age values
# table(rTrain[,5*ceiling(Age/5),by=list(Sex)]) # R version, included for reference

import numpy as np # nice package/library/module, we'll use it frequently!
# We'll use an anonymous or inline or "lambda" function (not quite the same thing, but we'll think about them as such for now)
rTrain.groupby(['Sex']).apply(lambda x: 5*np.ceil(x.Age/5)).value_counts().sort_index()
# Can you figure out what is happening above?
# not quite what we would have liked... where are the groups by Sex in the output?

# best to create the variable separately...
rTrain['Age5'] = 5*np.ceil(rTrain['Age']/5)
rTrain.groupby(['Sex']).Age5.value_counts().sort_index()
# more like it...
# now clean up the index and sorting as we did above

# similar view for Survived by Sex/Gender:
# table(rTrain[,Sex, by=list(Survived)]) # R version
rTrain.groupby(['Survived']).Sex.value_counts()

#%% Plotting / Graphics
# Both matplotlib and seaborn are quite powerful
import matplotlib.pyplot as plt
import seaborn as sns

#boxplot(rTrain[,Fare])
sns.distplot(rTrain['Age'], bins=20)
# what has happened? is your plot blank as well???

# hunch: this is probably due to missing values; let's try to remove them before plotting
AgeCond = rTrain['Age'].isnull() # define missing condition
AgeCond.value_counts() # missings vs non-missings
sns.distplot(rTrain.loc[~AgeCond, 'Age'], bins=20)

# a better view
sns.violinplot(rTrain['Age'])


# correlation
# var(rTrain[,Age], rTrain[,Fare], na.rm = T)  # R version
# !! careful with over-correlating... http://www.tylervigen.com/spurious-correlations !!

#corrplot(cor(rTrain), method='number')
sns.pairplot(rTrain)
# doesn't really do much, does it? Why?
# You guessed it - probably some missing values - by where the plotting stopped, missings are in "Age"

# We'll use the age-missing filter created above
# But first let's drop a variable, it should not be very relevant (?)
rTrain = rTrain.drop(columns=['Cabin'])
sns.pairplot(rTrain.loc[~AgeCond]) # aha, a bit better?
# note that is works for all columns - both numeric and character!

sns.pairplot(rTrain.loc[~AgeCond], kind="scatter", hue="Survived", markers=["o", "s"], palette="Set2")
sns.pairplot(rTrain.loc[~AgeCond], kind="scatter", hue="Survived", plot_kws=dict(s=80, edgecolor="white", linewidth=2.5))
#@@ play with these parameters, how to research what is available? think, search


# can we drop some (more) variables?
rTrain = rTrain.drop(columns=['PassengerId'])
sns.pairplot(rTrain.loc[~AgeCond], kind="scatter", hue="Survived", markers=["o", "s"], palette="Set2")

#%% Using seaborn for nicer boxplots
sns.boxplot(rTrain['Age'])
sns.boxenplot(rTrain['Age'])

#%% Pairwise & simple regression plotting

#plot(Survived ~ Fare + Pclass, data=rTrain)    # R version
sns.catplot(x='Fare', y='Pclass', hue='Survived', data=rTrain)
sns.catplot(x='Fare', y='Pclass', hue='Survived', data=rTrain, kind='violin')
# "violin" is not very helpful here, due to the data - but keep it in mind for later...

#@@@@ fix catplot
sns.lmplot()
# linear regression plots - in one plot
sns.lmplot(x='Fare', y='Pclass', hue='Survived', data=rTrain)
# linear regression plots - in two side-by-side plots
sns.lmplot(x='Fare', y='Pclass', col='Survived', data=rTrain)
#sns.catplot(rTrain['Fare'], rTrain['Pclass'], hue=rTrain['Survived'])
#@@@@ complete this one too
x = sns.pairplot(rTrain, hue='Survived', x_vars='Fare', y_vars='Pclass')
x.fig

# perhaps a bit of jittering would help
# We'll try to add random "noise" to the two variables Survived and Pclass
howMuchJitter = 0.2
import random

#!! Please note the difference between how we modify Pclass and Survived
rTrain['Pclass_j'] = rTrain['Pclass'] + random.gauss(0,howMuchJitter)
# ... and how we modify Survived
rTrain['Survived_j'] = rTrain['Survived'] 
rTrain['Survived_j'] += [random.gauss(0,howMuchJitter) for i in range(rTrain.shape[0])]

sns.pairplot(rTrain[['Survived_j','Pclass_j']])
# a bit too tiny.. let's change some size
sns.pairplot(rTrain[['Survived_j', 'Pclass_j']], height=5)
# HW: Analyze and describe the distribution of Survived_j and Pclass_j
# Hint: use describe(), boxplot, histogram(distplot)
# include the code and your conclusions in the online Text submission for the Assignment in Module 3

#%% imputation by mode
import scipy
agecond = rTrain.Age.isnull()
rTrain_Age_Mode = scipy.stats.mode(rTrain.loc[~agecond].Age)
rTrain_Age_Mode # we only need the mode, let's extract it
rTrain_Age_Mode.mode # almost there, just need the value from inside the array
rTrain_Age_Mode.mode[0] # finally...
rTrain_Age_Mode = rTrain_Age_Mode.mode[0] # yep, we keep only the relevant value
rTrain_Age_Mode

# now impute... but let's keep track of where we imputed, eh?
rTrain['Age_imputed'] = 0
rTrain.loc[agecond, 'Age_imputed'] = 1
# let's do a cross-tabulation to make sure we flagged exactly the missing Age records
pd.crosstab(rTrain['Age_imputed'], agecond)
# now we're ready for imputation
rTrain.loc[agecond, 'Age'] = rTrain_Age_Mode
# check with a histogram
sns.distplot(rTrain.Age)

#????????? mode(rTrain[,Age])   # not quite what we were expecting, right? see p.123 in DL for a "trick" which is not that good...

#%% mode of a list of numbers
testList = [1,2,3,4,1,2,3,1,2,1]
from collections import Counter
tcnts = Counter(testList)
tcnts.most_common()
tcnts.most_common(1)
tcnts.most_common(2)

# does this work on strings?
testList2 = [str(i) for i in testList]
tcnts2 = Counter(testList2)
tcnts2.most_common() # seem so

# another way:
from statistics import mode
mode(testList)
mode(testList2)

from scipy.stats import mode
mode(testList)
mode(testList2)
# try to tease the object above...
x = mode(testList)
type(x)
x.mode # and other ones...

# which one do you find more useful?

#%% imputation by mode
import scipy
agecond = rTrain.Age.isnull()
rTrain_Age_Mode = scipy.stats.mode(rTrain.loc[~agecond].Age)
rTrain_Age_Mode # we only need the mode, let's extract it
rTrain_Age_Mode.mode # almost there, just need the value from inside the array
rTrain_Age_Mode.mode[0] # finally...
rTrain_Age_Mode = rTrain_Age_Mode.mode[0] # yep, we keep only the relevant value
rTrain_Age_Mode

# now impute... but let's keep track of where we imputed, eh?
rTrain['Age_imputed'] = 0
rTrain.loc[agecond, 'Age_imputed'] = 1
# let's do a cross-tabulation to make sure we flagged exactly the missing Age records
pd.crosstab(rTrain['Age_imputed'], agecond)
# now we're ready for imputation
rTrain.loc[agecond, 'Age'] = rTrain_Age_Mode
# check with a histogram
sns.distplot(rTrain.Age)

#%%
# learning about identifying the position and value of the max (which.max)
# https://stackoverflow.com/questions/3989016/how-to-find-all-positions-of-the-maximum-value-in-a-list
# option 1
testList.index(max(testList)) # does this make sense?
# option 3
[i for i, j in enumerate(testList) if j == max(testList)]
# which one do you like?

# rerun above after extending the list with an extra (max) value
testList += [4]
testList
testList.index(max(testList)) # does this make sense?
[i for i, j in enumerate(testList) if j == max(testList)]
# now which one do you like? Are these equivalent?

#%% Preview / more informations about Lesson 3...

# one-way ANOVA
import statsmodels as sm

"""
Below's R code to be moved to Python...
Price by departure port
rTrain[,Embarked_f:=as.factor(Embarked)]
anova1 <- oneway.test(Fare ~ Embarked_f, data=rTrain)
anova1
summary(anova1)
# better option - "aov"
anova2 <- aov(Fare ~ Embarked, data = rTrain)
anova2
summary(anova2)

#==============================================================================
# Multiple Linear Regression
#==============================================================================
# Assumptions...
# ... and what do they mean?
library(alr4)
x <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = Rpdata )
# or: myfor <- as.formula(paste0("y ~ ",paste0("x",c(1:6), collapse=' + ')))

# does this look like a good model (numerically speaking)?
summary(x)
# how about we "dig deeper"?
residualPlots(x)
# some visual tests for patterns in residuals 
par(mfrow=c(2,2))
plot(x)

# Uh-oh! The residuals are not random! Or are they? What makes them non-random?
# Participate in the online discussion on randomness!

# now make 6 into a variable...
useNVars <- 6
myfor <- as.formula(paste0("y ~ ", paste0("x",c(1:useNVars), collapse = ' + ')))
myfor
mylm <- lm(myfor, data=Rpdata)
summary(mylm)
residualPlots(mylm)
# now replace in line 141 6 with 5, rerun lines 141-146, and draw some conclusions
"""
