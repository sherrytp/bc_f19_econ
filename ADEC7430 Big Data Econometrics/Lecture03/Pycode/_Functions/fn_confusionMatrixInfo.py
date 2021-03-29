# -*- coding: utf-8 -*-
"""
Created on Fri Feb 15 20:52:21 2019

@author: RV
"""

#%%
import pandas as pd
import numpy as np
import sklearn.metrics as skm

#%% Define two easy to understand series; a = actual (real), p = prediction/predicted
p = pd.Series([1,1,1,0,0,0,0,0,0,0])
a = pd.Series([1,0,0,1,1,1,0,0,0,0])
# assume 1 = True, 0 = False
# so we expect TP = 1, FP = 2, FN = 3, TN = 4 
#%% first attempt
skm.confusion_matrix(a,p)
# note the order in which the confusion matrix is laid out - it seems "0"/False is first?

skm.confusion_matrix(a,p,labels=[1,0]) # this time we indicate the priority/order of the "labels" (or levels)
# NOTE!!! that actuals are on the right side (horizontal/rows), predictions are on top (vertical/columns)

# How can we get more information? E.g. like the one displayed by R for confusionMatrix?
#p <- c(1,1,1,0,0,0,0,0,0,0)
#a <- c(1,0,0,1,1,1,0,0,0,0)
#
#library(caret)
#
#confusionMatrix(as.factor(p), as.factor(a))

#%%
def confusionMatrixInfo(p,a, labels = None):  # as what we learned in class 
    # those are test values and have to reset as notes afterwards
#    p = pd.Series([1,1,1,0,0,0,0,0,0,0])
#    a = pd.Series([1,0,0,1,1,1,0,0,0,0])
    labels = [1,0]
    
    x = skm.confusion_matrix(a,p,labels=labels)
    tp = x[0,0]
    tn = x[1,1]
    fp = x[1,0]
    fn = x[0,1]
    
    tsensitivity = tp/(tp+fn)   # 0.25
    tspecificity = tn/(tn + fp)  # 0.6666667
    # no information rate? representing the reality and should stay the same for all models. 
    tnir = (tp + fn)/x.sum()     # 0.4
    tnir = max(tnir, 1-tnir)
    # accuracy
    taccuracy = (tp + tn)/x.sum()
    # build a dictionary to store the results and return it 
    res = {'confusionMatrix':x,
           'accuracy': taccuracy,
           'no information rate': tnir,
           'sensitivity': tsensitivity,
           'specificity': tspecificity
           }
    return(res)

y = confusionMatrixInfo(p,a, labels=[1,0])
y

y['accuracy']
y['no information rate']
confusionMatrixInfo([1,1,1],[1,1,1])   # we get an error - why? 
confusionMatrixInfo([1,1,1],[1,0,1])
skm.confusion.matrix([1,1,1],[1,1,1])
skm.cofusion.matrix([1,1,1],[1,1,1], labels = [1,0])
confusionMatrixInfo([1,1,1],[1,1,1], labels = [1,5])