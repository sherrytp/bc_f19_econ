# -*- coding: utf-8 -*-
"""
Created on Sat Feb 16 00:38:49 2019

@author: RV
"""

#%%
import numpy as np
from sklearn.linear_model import Lasso
import matplotlib.pyplot as plt
#%%
useAlpha = 0.00001
lasso_1 = Lasso(alpha=useAlpha).fit(X, y)
print(lasso_1.coef_)
# OK, got the coefficients, but what do they represent???
print(X.columns)
# OK - can we align the two though?
# yes we can...
[i for i in zip(X.columns, lasso_1.coef_)]

# predictions
z = lasso_1.predict(X)

# attempt to identify a good cutoff
cutoffgrid = np.linspace(min(z), max(z), 50)

tcm1 = [confusionMatrixInfo(z < i, y, labels=[1, 0])['accuracy'] for i in cutoffgrid]

plt.figure()
plt.plot(tcm1)
plt.show()
#%%
# Ridge classifier
from sklearn.linear_model import RidgeClassifierCV as RCCV

RCCV_1 = RCCV(alphas=[np.exp(i) for i in range(-10, 0)]).fit(X, y)
RCCV_1.score(X, y) # not that great ?
zz = RCCV_1.predict(X)
confusionMatrixInfo(zz, y)

#%%
from sklearn.linear_model import ElasticNet as ENet

a = 0.0001
b = 0.0001
alpha = a+b
l1_ratio = a/(a+b)

ENet_1 = ENet(alpha=alpha, l1_ratio=l1_ratio).fit(X, y)

ENetz = ENet_1.predict(X)

cutoffgrid = np.linspace(min(ENetz), max(ENetz), 50)

tcm1 = [confusionMatrixInfo(ENetz < i, y, labels=[1, 0])['accuracy'] for i in cutoffgrid]
plt.figure()
plt.plot(tcm1)
plt.show()
