# -*- coding: utf-8 -*-
"""
Created on Fri Feb  8 19:56:13 2019

@author: RV
"""

#%% Setup
import os
import seaborn as sns     # good for 2D images 
import pandas as pd
import numpy as np
import datetime as DT
import matplotlib.pyplot as plt   # must have for 3D plots 

#%%
projFld = "/Users/apple/Desktop/ADEC7430 Big Data Econometrics/Lecture03"
codeFld = os.path.join(projFld, "PyCode")
fnsFld = os.path.join(codeFld,"_Functions")
outputFld = os.path.join(projFld, "Output")
rawDataFld = os.path.join(projFld, "RawData")
savedDataFld = os.path.join(projFld, "SavedData")

#%%
#==============================================================================
# Multiple Linear Regression
#==============================================================================
# Assumptions...
# ... and what do they mean?
Rpdata = pd.read_csv(os.path.join(rawDataFld, "alr4_Rpdata.csv"))

import statsmodels.api as sm
lmmodel = sm.OLS(Rpdata["y"], Rpdata[["x1","x2","x3","x4","x5","x6"]]).fit()

# does this look like a good model (numerically speaking)?
lmmodel.summary()    

# how about we "dig deeper"? - residual plots
lmmodel_fitted_y = lmmodel.fittedvalues
lmmodel_residuals = lmmodel.resid

import seaborn as sns
sns.residplot(lmmodel_fitted_y, 'y', data=Rpdata, lowess = True)
# Uh-oh! The residuals are not random! Or are they? What makes them non-random?
# Participate in the online discussion on randomness!
# Interesting page: https://www4.stat.ncsu.edu/~stefanski/nsf_supported/hidden_images/stat_res_plots.html

# HW follow up with comments on what is randomness, and how is this relevant to our analysis ????????????

#%% Back to Titanic data
# load dataset for training - don't forget to copy it from Lecture02/SavedData
rTrainFile = os.path.join(savedDataFld, "rTrain.pkl")
rTrain = pd.read_pickle(rTrainFile)
rTrain.shape
#%% let's clean up a bit the data
rTrain.dtypes
pd.isnull(rTrain).astype(int).aggregate(sum)
pd.isnull(rTrain).astype(int).aggregate(sum, axis = 0)

# this is a matrix, with 2 dimensions: rows and columns
# rows are axis = 0 (the first dimension), columns are axis = 1 (the second dimension)
#???????????????/ so "aggregate"

# drop some variables:
# in the absence of cabin location/map, the cabin (number) is hard to link to survival outcomes
# moreover there are 687 missings out of 891 records - a bit too much to "impute"...

#%%
# reusing the code from Lecture 02...
import scipy
agecond = rTrain.Age.isnull()
rTrain_Age_Mode = scipy.stats.mode(rTrain.loc[~agecond].Age)
rTrain_Age_Mode # we only need the mode, let's extract it
rTrain_Age_Mode.mode # almost there, just need the value from inside the array
type(rTrain_Age_Mode)
rTrain_Age_Mode.mode[0] # finally...
rTrain_Age_Mode = rTrain_Age_Mode.mode[0] # yep, we keep only the relevant value
rTrain_Age_Mode

#%% now impute... but let's keep track of where we imputed, eh?
rTrain['Age_imputed'] = 0
rTrain.loc[agecond, 'Age_imputed'] = 1
# let's do a cross-tabulation to make sure we flagged exactly the missing Age records
pd.crosstab(rTrain['Age_imputed'], agecond)
# now we're ready for imputation
rTrain.loc[agecond, 'Age'] = rTrain_Age_Mode
# check with a histogram
sns.distplot(rTrain.Age)

#%% how about Embarked?
rTrain.loc[pd.isnull(rTrain.Embarked)]
rTrain.loc[pd.isnull(rTrain.Embarked)].drop(columns={'Survived','Pclass','Embarked','Fare','Age_imputed'})
    # only drop from data viewing but not acutally drop the variables off the database 
# hmm - same ticket, seem like mother/daughter or so, same cabin, same ticket price
pd.crosstab(rTrain['Embarked'],rTrain['Pclass'])
# most first class passengers embarked in S - let's assign them there (other choices available)
rTrain.loc[pd.isnull(rTrain.Embarked), 'Embarked'] = 'S'

# also, the PassengerID is unique (and random) to passengers, so should not be related to survival outcome
rTrain = rTrain.drop(columns={'PassengerId', 'Cabin'})

# check...
rTrain.dtypes
pd.isnull(rTrain).astype(int).aggregate(sum)
rTrain.Ticket.value_counts()
# I'll drop ticket as well...
rTrain = rTrain.drop(columns={'Ticket'})
#==============================================================================

#%% sklearn modules
import sklearn.metrics
import sklearn.model_selection
import sklearn.preprocessing
# there are lots of models under sklearn - documentations for help 

# Logistic regression
from sklearn.linear_model import LogisticRegression as LogReg

data = rTrain.copy()
cat_vars=['Sex','Embarked']
for var in cat_vars:
    cat_list='var'+'_'+var
    cat_list = pd.get_dummies(data[var], prefix=var)  # this creates dummy variables from a categorical/characteristic variable 
# create a list of var_blabla, and go to the database "rTrain", pick up the variables in Embarked and fix them there. 
# eg. Embarked_C: ..Embarked_Q... 
    data1=data.join(cat_list)
    data=data1

# drop previous Embarked column 
data_vars=data.columns.values.tolist()
to_keep=[i for i in data_vars if i not in cat_vars]
data = data[to_keep].copy()

data.dtypes
X = data.drop(columns={'Survived'})
y = data['Survived']
logRegM = LogReg(random_state = 0).fit(X,y)
logRegM = LogReg(random_state = 0, solver ='lbfgs', max_iter=1000).fix(X,y) 
# go 1000 times to find the closest estimate around the edge. 

# What is this object 
type(logRegM)  # no clue..
# What can it do? Here is what fuctions you can do with this? 
logRegM.__dir__()

z = logRegM.predict(X)
from sklearn.metrics import confusion_matrix as cm
cm(y, z) # not that great...
# better print (load fn_confusionMatrixInfo)
confusionMatrixInfo(z,y)
#@@ to do - try to package that into a function 
# which tries various combinations of predictors? 

#%% ------- this is the R version codes ---------
# training error
# logreg.probs <- predict(logreg, type='response')
# tr.preds <- as.integer(logreg.probs > 0.5) # this is a cutoff (arbitrarily chosen by me !!!)

# install.packages("caret")
# install.packages("e1071")
# library(caret)
# library(e1071)

# confusionMatrix(tempData[,Survived], tr.preds)
# length(tempData[,Survived])
# length(tr.preds)

# ------ end of R version ----------

#%% Break - sklearn.LabelEncode
from sklearn.preprocessing import LabelEncoder as LabEnc
LabEnc().fit(rTrain["Embarked"]).transform(rTrain['Embarked'])
#%% LDA/QDA
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis as QDA

LDAm = LDA().fit(X,y) # see the warning?
# drop columns known to be collinear
badcols = ['Sex_male', 'Embarked_S']
Xg = X.drop(columns=badcols)
LDAm = LDA().fit(Xg,y)
LDAz = LDAm.predict(Xg)
cm(LDAz, y) # slightly better...
confusionMatrixInfo(y, LDAz) # something strange with no information rate? aha, switched predicted and actuals...
confusionMatrixInfo(LDAz, y) 

QDAm = QDA().fit(Xg,y)
QDAz = QDAm.predict(Xg)
QDAzp = QDAm.predict_proba(Xg)
cm(QDAz, y) # slightly better again...
print(skm.classification_report(y, QDAz))
confusionMatrixInfo(QDAz,y)


# ROC curve
import sklearn.metrics as skm
skm.roc_curve(y, QDAz)
#=================
# Compute ROC curve and ROC area for each class
fpr = dict()
tpr = dict()
roc_auc = dict()
n_classes = 2 # binary classification

for i in range(n_classes):
    fpr[i], tpr[i], _ = skm.roc_curve(y==i, QDAzp[:,i])
    roc_auc[i] = skm.roc_auc_score(y==i, QDAzp[:,i])
# technically we don't need both curves, one will do...
    
# further reading: https://machinelearningmastery.com/roc-curves-and-precision-recall-curves-for-classification-in-python/
# see references in there for ROC vs Precision-Recall curve "when to" arguments

#Plot of a ROC curve for a specific class

#@@ Quiz - make this into a function 
# inputs: 
#    a list of probabilitieis (predictions) for an outcome 
#    a list of outcomes (1/0)
# output: 
#    a plot of ROC 
# keys: 
#    predicted -> predicted.sort 
#    then determine a line to sepera the sorted recording to the outcomes 
#    closest point to "Receiver operating characteristic" ROC

plt.figure()
lw = 2
plt.plot(fpr[1], tpr[1], color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc[1])
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic example')
plt.legend(loc="lower right")
plt.show()




#=================
#@@ Quiz - how to package the models to test easier - continue here 
# KNN
# annoyingly (but justifiably) it requires testing dataset to make predictions

from sklearn.neighbors import KNeighborsClassifier as knn
knn3 = knn(n_neighbors = 3).fit(X, y)
KNNz = knn3.predict(X)
KNNzp = knn3.predict_proba(X)

cm(y, KNNz)
confusionMatrixInfo(KNNz, y, labels=[1,0])


# but we did this disregarding scale - which may lead to "neighbors" being mis-identified
# So first scale (normalize) the variables to take this "effect" out

XS = sklearn.preprocessing.scale(X, axis = 0)
XS = pd.DataFrame(XS)
XS.columns = X.columns

knns3 = knn(n_neighbors = 3).fit(XS, y)
KNNSz = knns3.predict(XS)
KNNSzp = knns3.predict_proba(XS)

cm(y, KNNSz)
confusionMatrixInfo(KNNSz, y, labels=[1,0])

knns5 = knn(n_neighbors = 5).fit(XS, y)
KNNSz = knns5.predict(XS)
KNNSzp = knns5.predict_proba(XS)

cm(y, KNNSz)
confusionMatrixInfo(KNNSz, y, labels=[1,0])

# Already in a bit of trouble with notation etc.
