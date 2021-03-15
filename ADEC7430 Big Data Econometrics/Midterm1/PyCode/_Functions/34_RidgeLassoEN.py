# -*- coding: utf-8 -*-
"""
Created on Sat Feb 23 00:14:38 2019

@author: RV
Purpose: use Ridge, Lasso and Elastic Net models on Titanic data
"""


#%% Lasso
from sklearn.linear_model import Lasso
useAlpha = 0.00001

lasso_1 = Lasso(alpha = useAlpha).fit(X, y)
print(lasso_1.coef_)
# OK, got the coefficients, but what do they represent???
X.columns
# OK - can we align the two though?
#??????? yes we can... what is zip function? 
[i for i in zip(X.columns, lasso_1.coef_)]

# predictions
lasso_1_is_pred = lasso_1.predict(X)

# attempt to identify a good cutoff
cutoffgrid = np.linspace(min(lasso_1_is_pred), max(lasso_1_is_pred), 100)

tcm1 = [confusionMatrixInfo(lasso_1_is_pred < i, y, labels=[1,0])['accuracy'] for i in cutoffgrid]

plt.figure()
plt.plot(tcm1)
plt.show()
#%%
# Ridge classifier
from sklearn.linear_model import RidgeClassifierCV as RCCV

RCCV_1 = RCCV(alphas=[np.exp(i) for i in np.linspace(-10,0,50)]).fit(X,y)
RCCV_1.score(X,y) # not that great ?
RCCV_1_is_pred = RCCV_1.predict(X)
confusionMatrixInfo(RCCV_1_is_pred,y)

# attempt to identify a good cutoff
cutoffgrid = np.linspace(min(RCCV_1_is_pred), max(RCCV_1_is_pred), 100)

tcm1 = [confusionMatrixInfo(RCCV_1_is_pred < i, y, labels=[1,0])['accuracy'] for i in cutoffgrid]

plt.figure()
plt.plot(tcm1)
plt.show()

from sklearn.datasets import load_breast_cancer
from sklearn.linear_model import RidgeClassifierCV
X, y = load_breast_cancer(return_X_y=True)
clf = RidgeClassifierCV(alphas=[1e-3, 1e-2, 1e-1, 1]).fit(X, y)
clf.score(X, y) 
#%%
from sklearn.linear_model import ElasticNet as ENet

a = 0.0001
b = 0.0001
alpha = a+b
l1_ratio = a/(a+b)

ENet_1 = ENet(alpha = alpha, l1_ratio= l1_ratio).fit(X,y)

ENet_1_is_pred = ENet_1.predict(X)

cutoffgrid = np.linspace(min(ENet_1_is_pred), max(ENet_1_is_pred), 100)

tcm1 = [confusionMatrixInfo(ENet_1_is_pred < i, y, labels=[1,0])['accuracy'] for i in cutoffgrid]
plt.figure()
plt.plot(tcm1)
plt.show()
