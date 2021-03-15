# -*- coding: utf-8 -*-
"""
Created on Sat Mar 16 10:20:17 2019

@author: RV
"""
#%%
import os
import pandas as pd
import numpy as np
import random
import sys

import scipy

from matplotlib import pyplot as plt
from sklearn.metrics import confusion_matrix as skm_conf_mat

#%%
projFld = "C:/Users/RV/Documents/Teaching/2019_01_Spring/ADEC7430_Spring2019/Lecture07"
codeFld = os.path.join(projFld, "PyCode")
fnsFld = os.path.join(codeFld,"_Functions")
outputFld = os.path.join(projFld, "Output")
rawDataFld = os.path.join(projFld, "RawData")
savedDataFld = os.path.join(projFld, "SavedData")

fnList = [
         "fn_logMyInfo"
        ,"fn_confusionMatrixInfo"
        ,"fn_MakeDummies"
        ,"fn_InfoFromTree"
        ] 
for fn in fnList:
    exec(open(os.path.join(fnsFld, fn + ".py")).read())

#@@ see how the functions are documented for quick review prior to use - help your own future self...
print(fn_MakeDummies.__doc__)
#%%
CarseatsFile = os.path.join(rawDataFld, "Carseats.csv")
Carseats = pd.read_csv(CarseatsFile)

Carseats.head()
Carseats.describe() #
# @@RV how to emulate str(Carseats)
Carseats.describe(include='all') #full listing, including character columns
Carseats.dtypes

# split Carseats data into 70-30
random.seed(2019)
rndindex = [random.uniform(0,1) for x in range(Carseats.shape[0])]
rndindex = [True if x < 0.7 else False for x in rndindex]

# transform problem into a classification problem, predicting Sales > 8, not actual Sales
Carseats['High'] = "Yes"
Carseats.loc[Carseats['Sales']<=8, "High"] = "No"
Carseats.High.value_counts()

Carseats_train = Carseats.loc[rndindex]
Carseats_test = Carseats.loc[[not x for x in rndindex]]

#%% exploration step

# base this solely on Carseats_train

#sapply(Carseats_train, function(x) sum(is.na(x))) # R version
pd.isnull(Carseats_train).astype(int).aggregate(sum, axis=0) # Python version

#@@ Think ahead: what to do if we have missings in the (future) test datasets?
#@@ - store the "mode" (or any other measure we want) to be reused for test data
#@@ - must go through each variable, and collect one value that will be the default for any missings going forward
#@@ - this is independent of whether there are actual missing in the training data or not

# we must exit this step with defaultsForMissingVariablesFromTraining - a dictionary of variable<->fill-in value


def fn_GetModeOfVars(dsin):
    tdict = {}
    for tvar in dsin.columns:
        tdict.update({tvar:scipy.stats.mode(dsin[tvar]).mode[0]})
        #Note: the only new thing here is "adding to" (updating) a dictionary; make sure you understand what the line above does
    return(tdict)
defaultsForMissingVariablesFromTraining  = fn_GetModeOfVars(Carseats_train)

#%% data prep step

def prepMyData(whichData, useVars=None, defaultsForMissingVariables=None, varsToDummies=None):

#@@ plan ahead - create variables that existed in the training data, if they are not present in the test data
    tMissingVars = [x for x in useVars if x not in whichData.columns]
    for tvar in tMissingVars:
        whichData[tvar] = np.nan # we're leaving it ambiguous - "not a number"
        
#@@ from now on focus only on the variables to be used (useVars) - drop the rest
    whichData = whichData[useVars].copy()
    
#@@ ignore the new variables that may exist in the new data - can't do anything with them
    
#@@ fill in missing values in the variables for which we have defaults
    tvarsToFill = [x for x in useVars if x in list(defaultsForMissingVariables.keys())]
    for tvar in tvarsToFill:
        whichData.loc[pd.isna(whichData[tvar]), tvar] = defaultsForMissingVariables[tvar]
        
#@@ at the end create the dummies as well
    whichData = fn_MakeDummies(whichData, varsToDummies)
    
    return(whichData)
    
#%% prep the Train data 
Carseats_train_prepped = prepMyData(Carseats_train.copy()
           ,useVars = ['Sales', 'CompPrice', 'Income', 'Advertising',
                      'Population', 'Price','ShelveLoc', 'Age',
                      'Education', 'Urban', 'US', 'High']
           ,defaultsForMissingVariables = defaultsForMissingVariablesFromTraining
           ,varsToDummies=['ShelveLoc','Urban','US'])


#%% train the model

# fit a tree
from sklearn.tree import DecisionTreeClassifier as DTree
from sklearn import tree as treemodule

train_X = Carseats_train_prepped.drop(columns={'High','Sales'})
train_Y = Carseats_train_prepped['High']

tree = DTree(min_samples_leaf=30) #@@ try other values !!!
tree.fit(train_X, train_Y)

#%% How does this tree look like?
#@@ RV to insert code for plotting trees

from graphviz import Source
Source(treemodule.export_graphviz(
        tree
        , out_file=None
        , feature_names=train_X.columns
        , filled = True
        , proportion = True #@@ try False and understand the differences
        )
)

#%% prep the test data as well before using it to predict
Carseats_test_prepped = prepMyData(Carseats_test.copy()
           ,useVars = ['Sales', 'CompPrice', 'Income', 'Advertising',
                      'Population', 'Price','ShelveLoc', 'Age',
                      'Education', 'Urban', 'US', 'High']
           ,defaultsForMissingVariables = defaultsForMissingVariablesFromTraining
           ,varsToDummies=['ShelveLoc','Urban','US'])
test_X = Carseats_test_prepped.drop(columns={'High','Sales'})
pred_Y = tree.predict(test_X)

#%% accuracy measures on testing data
confusionMatrixInfo(pred_Y, Carseats_test_prepped['High'])


#%% Features importances - which variables were most impactful
feature_importances = pd.DataFrame(tree.feature_importances_,
                                   index = train_X.columns,
                                    columns=['importance'])\
                                   .sort_values('importance',
                                                ascending=False)
feature_importances

#%%
#@@RV figure out how to replace this summary(tree.carseats)
print(fn_InfoFromTree.__doc__) # we imported it above...
fn_InfoFromTree(tree)
fn_InfoFromTree(tree, test_X)
