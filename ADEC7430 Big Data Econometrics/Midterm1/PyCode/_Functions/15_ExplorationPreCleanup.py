# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 09:27:03 2019

@author: RV
Purpose: exploration of (training) dataset to help inform the cleaning function
Outputs: as needed to document the decisions
"""
#%% load dataset for training - don't forget to copy it from Lecture02/SavedData
rTrainFile = os.path.join(savedDataFld, "rTrain.pkl")
rTrain = pd.read_pickle(rTrainFile)
rTrain.shape

#%% let's understand up a bit the data
rTrain.dtypes
# many times, you don't need it b/c you may have data coming from deferent sources and you need to specify which origin it comes
# only our ways of understanding the database 
pd.isnull(rTrain).astype(int).aggregate(sum, axis = 0)

# this is a matrix, with 2 dimensions: rows and columns
# rows are axis = 0 (the first dimension), columns are axis = 1 (the second dimension)
# so "aggregate" has 2 arguments: "HOW" (operation), and "WHERE" (the axis)

# drop some variables:
# in the absence of cabin location/map, the cabin (number) is hard to link to survival outcomes
# moreover there are 687 missings out of 891 records - a bit too much to "impute"...

#%%
import scipy
import pandas as pd 

agecond = rTrain.Age.isnull()
agecond
rTrain_Age_Mode = scipy.stats.mode(rTrain.loc[~agecond].Age) #~ is the negation operator in pandas indices
rTrain_Age_Mode # we only need the mode, let's extract it
rTrain_Age_Mode.mode # almost there, just need the value from inside the array
rTrain_Age_Mode.mode[0] # finally...
rTrain_Age_Mode = rTrain_Age_Mode.mode[0] # yep, we keep only the relevant value
rTrain_Age_Mode

#%% define dictionary of values that need to be remembered
cleanupValues = {}

# need to save this value for reference    24.0
cleanupValues['Age.null.imputation'] = rTrain_Age_Mode

# cleanupValues['Age.null.imputation'] error is NoneType 
#??????????? NoneType object is not subscriptable 
# Have to change it to def function - cleanupRawData(newData, cleanupValues = cleanupValues)

#%% how about Embarked?
# get a new column Age_imputed by assigning 0 to missing variables and 1 to none
rTrain['Age_imputed'] = 0
rTrain.loc[agecond, 'Age_imputed'] = 1

rTrain.loc[pd.isnull(rTrain.Embarked)]
rTrain.loc[pd.isnull(rTrain.Embarked)].drop(columns={'Survived','Pclass','Embarked','Fare','Age_imputed'})
# hmm - same ticket, seem like mother/daughter or so, same cabin, same ticket price
pd.crosstab(rTrain['Embarked'],rTrain['Pclass'])
# most first class passengers embarked in S - let's assign them there (other choices available)
# most passengers embarked in S in all three situations, from the crosstable
# modes for Pclass(1,2,3) are all S

# rTrain.loc[pd.isnull(rTrain.Embarked), 'Embarked'] = 'S'   # assign 'S' to missing Embarked

# save this decision for application in the cleaning step data
cleanupValues['Embarked.null.imputation'] = 'S'

#%% Quiz for Monday
#@@ if this imputation would be passenger class (Pclass) sensitive, we would have to store 3 values
# START 
# Find out if there are missing values in Pclass or Embarked - but already checked on line 19
# pclasscond = rTrain.Pclass.isnull()
import scipy
import pandas as pd 

embarkedcond = rTrain.Embarked.isnull()
# pclasscond.value_counts()
embarkedcond.value_counts()
# therefore, there are no missing values in Pclass or Embarked

# calculate the modes for Pclass 
rTrain_Pclass_Mode = scipy.stats.mode(rTrain.Pclass)
rTrain_Pclass_Mode = rTrain_Pclass_Mode.mode[0]

# split the dataframe into three groups based on values of Pclass (1,2,3)
rTrain_Pclass1 = rTrain.loc[rTrain['Pclass']==1] 
rTrain_Pclass2 = rTrain.loc[rTrain['Pclass']==2] 
rTrain_Pclass3 = rTrain.loc[rTrain['Pclass']==3] 

# calculate modes and store modes for each Pclass group
rTrain_Embarkcond_Pclass1_Mode = scipy.stats.mode(rTrain_Pclass1.loc[~embarkedcond].Embarked)
rTrain_Embarkcond_Pclass1_Mode = rTrain_Embarkcond_Pclass1_Mode.mode[0]
rTrain_Embarkcond_Pclass1_Mode
rTrain_Embarkcond_Pclass2_Mode = scipy.stats.mode(rTrain_Pclass2.loc[~embarkedcond].Embarked)
rTrain_Embarkcond_Pclass2_Mode = rTrain_Embarkcond_Pclass2_Mode.mode[0]
rTrain_Embarkcond_Pclass2_Mode
rTrain_Embarkcond_Pclass3_Mode = scipy.stats.mode(rTrain_Pclass3.loc[~embarkedcond].Embarked)
rTrain_Embarkcond_Pclass3_Mode = rTrain_Embarkcond_Pclass3_Mode.mode[0]
rTrain_Embarkcond_Pclass3_Mode

# restore values for application in the cleaning step data 
rTrain.loc[pd.isnull(rTrain_Pclass1.Embarked), 'Embarked'] = 'S'
cleanupValues['Embarked.Pclass1.imputation'] = rTrain_Embarkcond_Pclass1_Mode
rTrain.loc[pd.isnull(rTrain_Pclass2.Embarked), 'Embarked'] = 'S' 
cleanupValues['Embarked.Pclass2.imputation'] = rTrain_Embarkcond_Pclass2_Mode
rTrain.loc[pd.isnull(rTrain_Pclass3.Embarked), 'Embarked'] = 'S'
cleanupValues['Embarked.Pclass3.imputation'] = rTrain_Embarkcond_Pclass3_Mode

# assign reference values back to rTrain missing embarked data - invalid syntax?
# rTrain.loc['Pclass' = 1 & pd.isnull(rTrain.Embarked), 'Embarked'] = cleanupValues['Embarked.Pclass1.imputation']
# rTrain.loc['Pclass' = 2 & pd.isnull(rTrain.Embarked), 'Embarked'] = cleanupValues['Embarked.Pclass2.imputation']
# rTrain.loc['Pclass' = 3 & pd.isnull(rTrain.Embarked), 'Embarked'] = cleanupValues['Embarked.Pclass3.imputation']

# drop three useless groups, but why cannot delete when running the entire file? 
del rTrain_Pclass1, rTrain_Pclass2, rTrain_Pclass3

# alternatively, can run line 102, 104, 106 to assign values to missing embarked data 
# END 

#%% Impute Fare 
xcond = pd.isnull(newData.Fare)
# MOVE TO 15 

cleanupValues['Fare.null.imputation'] 
Fare_Mode = scipy.stats.mode(newData.loc[~xcond].Fare)

#%%
# I'll drop ticket as well...
rTrain = rTrain.drop(columns={'Ticket', 'Name'})
#==============================================================================

# also, the PassengerID is unique (and random) to passengers, so should not be related to survival outcome
rTrain = rTrain.drop(columns={'PassengerId', 'Cabin'})

# check...
rTrain.dtypes
pd.isnull(rTrain).astype(int).aggregate(sum)
rTrain.Ticket.value_counts()

#%% save some objects for reuse (e.g. in PrepCleanupDataset)
pd.to_pickle(obj = cleanupValues, path = cleanupValuesFilePath)
