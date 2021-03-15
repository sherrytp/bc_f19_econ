# -*- coding: utf-8 -*-
"""
Created on Thu Mar 28 09:20:39 2019

@author: brian
"""

#%% gradient boosting
from sklearn.ensemble import GradientBoostingClassifier as GBClass

# first direct way
model_gbc = GBClass(learning_rate = 0.1, n_estimators=100, subsample=1.0)

params = {'n_estimators':100, 'subsample':1.0, 'learning_rate':0.1}
# 1' with 100 estimators, LR=0.1
# 6' with 200 estimators, LR=0.1
# to do: play with a grid of parameters (overnight), saving all models / plots for review/comparison
params = dict(params) # is this needed?

model_gbc = GBClass(**params)

t0 = DT.datetime.now()
model_gbc.fit(X_train, y_train3)
t1 = DT.datetime.now()
print('GBC took ' + str(t1-t0))

z_gbc = model_gbc.predict_proba(X_test)[:,1]

#ROC
fpr_gbc, tpr_gbc, thresh_gbc = skm.roc_curve(y_test3, z_gbc)
plt.figure(3)
plt.plot(fpr_gbc, tpr_gbc, 'r-')

# AUC
skm.auc(fpr_gbc,tpr_gbc)

# Deviance (see https://scikit-learn.org/stable/auto_examples/ensemble/plot_gradient_boosting_regularization.html#sphx-glr-auto-examples-ensemble-plot-gradient-boosting-regularization-py)
# compute test set deviance
test_deviance = np.zeros((params['n_estimators'],), dtype=np.float64)

for i, y_pred in enumerate(model_gbc.staged_decision_function(X_test)):
    # clf.loss_ assumes that y_test[i] in {0, 1}
    test_deviance[i] = model_gbc.loss_(y_test3, y_pred)

plt.plot((np.arange(test_deviance.shape[0]) + 1)[::1], test_deviance[::1],
        '-', color='red', label=str(params))
#plt.close()


