# -*- coding: utf-8 -*-
"""
Created on Thu Mar 28 09:35:12 2019

@author: brian
"""

#%%========= SVM =================
# we'll need to sample, hard to train on more than 20K records
ssize = 5000
samplerows = np.random.choice(range(len(y_train3)), size=ssize, replace=False)
X_train_svm = X_train.iloc[samplerows,]
y_train3_svm = y_train3.iloc[samplerows]

#%% SVM - attempt 1 - tried poly & rbf; LinearSVC doesn't produce probabilities
from sklearn import svm
model_svm = svm.SVC(C=1,gamma='auto', degree=3,kernel='poly')
#model_svm = svm.LinearSVC(max_iter=1000)

t0=DT.datetime.now()
model_svm.fit(X_train_svm, y_train3_svm)
t1=DT.datetime.now()
print('svm training took ' + str(t1-t0))

z_svm = model_svm.predict_proba(X_test)[:,1]
#%% SVM - attempt 2 - by the book https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.GridSearchCV.html#sklearn.model_selection.GridSearchCV
from sklearn.model_selection import GridSearchCV
parameters = {'kernel':('linear', 'rbf'), 'C':list(np.logspace(-10,3,14))}
svc = svm.SVC(gamma="scale")
cvsvm = GridSearchCV(svc, parameters, cv=5, n_jobs=2)
cvsvm.fit(X_train_svm, y_train3_svm)

ww = cvsvm.cv_results_
ww.keys()
ww['mean_test_score']
ww['params']
cvsvm.best_index_
pd.DataFrame(ww).transpose()

#%%
model_svm = svm.SVC(C=10,gamma='scale', kernel='rbf')
#model_svm = svm.LinearSVC(max_iter=1000)

t0=DT.datetime.now()
model_svm.fit(X_train_svm, y_train3_svm)
t1=DT.datetime.now()
print('svm training took ' + str(t1-t0))

z_svm = model_svm.predict_proba(X_test)[:,1]
#%%
#ROC
fpr_svm, tpr_svm, thresh_svm = skm.roc_curve(y_test3, z_svm)
plt.figure(3)
plt.plot(fpr_svm, tpr_svm, 'r-')

# AUC
skm.auc(fpr_svm,tpr_svm)

#%% (bonus) using GPU
