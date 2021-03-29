# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:23:09 2019

@author: RV
"""

#%% import Boston data (from R)
Boston = pd.read_csv(os.path.join(rawDataFld, "Boston.csv"))

kdigits = pd.read_csv(os.path.join(rawDataFld, "KDigits_train.csv"))

#%%
# split Train/Test
# we used in prior lessons a vector of True/False to select rows for Train, the rest into Test
# now use the built-in method from sklearn
from sklearn.model_selection import train_test_split as skl_traintest_split
X = kdigits.copy().drop(columns={'label'})
y = kdigits['label']

X_train, X_test, y_train, y_test = skl_traintest_split(X, y, test_size=0.20, random_state=2019)
# test_size - if between 0 and 1, represents the proportion of data to be reserved for testing
#           - if > 1 and integer, represents the count of rows to be reserved for testing

print(X_train.shape)
print(X_test.shape)
print(y_train.value_counts())
print(y_test.value_counts())

#%% view help for data
plt.imshow(np.array(X_train.iloc[200], dtype='uint8').reshape((28,28)),cmap='gray')

def viewDigits(whichSeries, threshold=None, cntfig=None):
    whichSeries = np.array(whichSeries, dtype='uint8')
    if threshold is not None:
        whichSeries = (whichSeries > threshold).astype(int)
    if cntfig is not None:
        plt.figure(cntfig)
    plt.imshow(whichSeries.reshape((28,28)), cmap='gray')

viewDigits(X_train.iloc[200], 100, cntfig=2)
viewDigits(X_train.iloc[200], 10, cntfig = 3)
viewDigits(X_train.iloc[200], 200, cntfig = 4)

#@@ RV to implement subplots here, to show 4 different cutoffs for B/W mapping at one time

#%% Build a model to recognize / distinguish 3 from the other data
y_train3 = (y_train == 5).astype(int)
y_train3.value_counts()
y_test3 = (y_test == 5).astype(int)
y_test3.value_counts()

#%% randomForest
from sklearn.ensemble import RandomForestClassifier as RFClass
model_rf = RFClass(n_estimators = 400, max_depth=6, random_state=2019, n_jobs=6)

t0 = DT.datetime.now()
model_rf.fit(X_train, y_train3)
t1 = DT.datetime.now()
print('training took ' + str(t1-t0))

z_test = model_rf.predict_proba(X_test)[:,1]

# ROC
fpr_rf, tpr_rf, thresh_rf = skm.roc_curve(y_test3, z_test)
plt.figure(2)
plt.plot(fpr_rf, tpr_rf, 'r-')

# AUC
skm.auc(fpr_rf,tpr_rf)

# cost-based optimum
fpc = 1000
fnc = 10
totalcost = fpc*fpr_rf + [fnc*(1-x) for x in tpr_rf]

# position of min?
minpos = np.argmin(totalcost)
thresh_rf[minpos]
plt.text(fpr_rf[minpos], tpr_rf[minpos], str(round(thresh_rf[minpos],2)))
#@@RV - make text larger, further from the actual point

plt.close()

#@@ Class part of midterm:==========================
# 1. scale all input vectors to use the full spectrum 0-255 - so expect min=0, max=255 for each row of X_train
# 2. subsample the non-"5" records to balance the training dataset - expect about 50-50% in y_train3.value_counts()
# Then, redo the steps above - i.e. retrain and review performance based on training with the balanced dataset
#===================================================


# look at feature importances
feature_importances = pd.DataFrame(model_rf.feature_importances_,
                                   index = X_train.columns,
                                    columns=['importance'])\
                                   .sort_values('importance',
                                                ascending=False)
feature_importances
plt.plot(feature_importances[:20])

# plot one tree

from graphviz import Source
from sklearn import tree as treemodule
Source(treemodule.export_graphviz(
        model_rf.estimators_[1]
        , out_file=None
        , feature_names=X_train.columns
        , filled = True
        , proportion = True #@@ try False and understand the differences
        )
)

#%% gradient boosting
from sklearn.ensemble import GradientBoostingClassifier as GBClass

# first direct way
model_gbc = GBClass(learning_rate = 0.1, n_estimators=100, subsample=1.0)

params = {'n_estimators':100, 'subsample':1.0, 'learning_rate':0.1}
# 1' with 100 estimators, LR=0.1
# 6' with 200 estimators, LR=0.1
# Midterm: to do: play with a grid of parameters (overnight), saving all models / plots for review/comparison
params = dict(params) # is this needed?

model_gbc = GBClass(**params)

t0 = DT.datetime.now()
model_gbc.fit(X_train, y_train3)
t1 = DT.datetime.now()
print('GBC took ' + str(t1-t0))

z_gbc = model_gbc.predict_proba(X_test)[:,1]

#ROC
import matplotlib.pyplot as plt
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


#%%========= SVM =================
# we'll need to sample, hard to train on more than 20K records
ssize = 5000
samplerows = np.random.choice(range(len(y_train3)), size=ssize, replace=False)
X_train_svm = X_train.iloc[samplerows,]
y_train3_svm = y_train3.iloc[samplerows]

#%% SVM - attempt 1 - tried poly & rbf; LinearSVC doesn't produce probabilities
from sklearn import svm
model_svm = svm.SVC(C=1, gamma='auto', degree=3, kernel='rbf')
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
