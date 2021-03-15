# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 23:23:57 2019

@author: RV
Logistic regression for Titanic data
"""

#%% sklearn modules
import sklearn.metrics
import sklearn.model_selection
import sklearn.preprocessing
from sklearn.linear_model import LogisticRegression as LogReg

#%% Logistic regression
# if ever need to change X and Y and then save to pickle 

X.to_pickle(os.path.join(savedDataFld, 'trainPredictorsDummies.pkl'))
y.to_pickle(os.path.join(savedDataFld, 'trainOutcome.pkl'))

X = pd.read_pickle(os.path.join(savedDataFld, 'trainPredictorsDummies.pkl'))
y = pd.read_pickle(os.path.join(savedDataFld, 'trainOutcome.pkl'))
train2_X = pd.read_pickle(os.path.join(savedDataFld, 'testPredictorsDummies.pkl'))
train2_y = pd.read_pickle(os.path.join(savedDataFld, 'testOutcome.pkl'))
X_test = pd.read_pickle(os.path.join(savedDataFld, 'actualPredictorsDummies.pkl'))

#%% fit the model on 70% and test on 30%  
logRegM = LogReg(random_state = 0, solver='lbfgs', max_iter=1000).fit(X,y)
train2_z = logRegM.predict(train2_X)

confusionMatrixInfo(train2_z,train2_y)

"""
{'confusionMatrix': array([[135,  31],
        [ 28,  73]]),
 'accuracy': 0.7790262172284644,
 'no information rate': 0.6217228464419475,
 'sensitivity': 0.8132530120481928,
 'specificity': 0.7227722772277227}
"""
