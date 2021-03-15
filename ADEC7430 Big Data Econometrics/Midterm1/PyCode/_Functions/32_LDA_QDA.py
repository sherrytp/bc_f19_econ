# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 23:47:41 2019

@author: RV
LDA / QDA models for Titanic data
"""

#%% Read from the history of X, y and Test_X
X = pd.read_pickle(os.path.join(savedDataFld, 'trainPredictorsDummies.pkl'))
y = pd.read_pickle(os.path.join(savedDataFld, 'trainOutcome.pkl'))
train2_X = pd.read_pickle(os.path.join(savedDataFld, 'testPredictorsDummies.pkl'))
train2_y = pd.read_pickle(os.path.join(savedDataFld, 'testOutcome.pkl'))
X_test = pd.read_pickle(os.path.join(savedDataFld, 'actualPredictorsDummies.pkl'))

#%% LDA fit the 70% model
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
# drop columns known to be collinear
badcols = ['Sex_male', 'Embarked_S']
Xg = X.drop(columns=badcols)

LDAm = LDA().fit(Xg,y)

#%% LDA predict on 30% trained data 
train2_Xg = train2_X.drop(columns = badcols)
LDAz = LDAm.predict(train2_Xg)
LDAzp = LDAm.predict_proba(train2_Xg)

confusionMatrixInfo(LDAz, train2_y) 
"""
{'confusionMatrix': array([[133,  33],
        [ 29,  72]]),
 'accuracy': 0.7677902621722846,
 'no information rate': 0.6217228464419475,
 'sensitivity': 0.8012048192771084,
 'specificity': 0.7128712871287128}
"""

#%% QDA fit the 70% model 
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis as QDA
QDAm = QDA().fit(Xg,y)

#%% predict on 30% trained data 
QDAz = QDAm.predict(train2_Xg) 
QDAzp = QDAm.predict_proba(train2_Xg)

print(skm.classification_report(train2_y, QDAz))
confusionMatrixInfo(QDAz,train2_y)

"""
{'confusionMatrix': array([[134,  32],
        [ 26,  75]]),
 'accuracy': 0.7827715355805244,
 'no information rate': 0.6217228464419475,
 'sensitivity': 0.8072289156626506,
 'specificity': 0.7425742574257426}
"""
