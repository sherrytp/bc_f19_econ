# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 08:50:01 2019

@author: RV
Purpose: Build function to prepare train data for model estimation
Inputs:  DataFrame of raw data imported
Outputs: DataFrame (saved) of data cleaned/prepared
         Cleaning function saved as an object - ready to be applied to a test datase 
    
All other green parts need changing and moving into other parts of PyCode. 
"""

#%% what data will this be applied to? 
newData = rTrain.copy()  # in practice, this is not a copy of Train, but a new dataset 

cleanupValues = pd.read_pickle(cleanupValuesFilePath)
"""
#%% load dataset for training - don't forget to copy it from Lecture02/SavedData
rTrainFile = os.path.join(savedDataFld, "rTrain.pkl")
rTrain = pd.read_pickle(rTrainFile)
rTrain.shape

#%% let's understand a bit the data
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

#%% define dictionary of values that need to be remembered 
cleanupValues = {}

#%%
# reusing the code from Lecture 02...
import scipy
agecond = rTrain.Age.isnull()
agecond
rTrain_Age_Mode = scipy.stats.mode(rTrain.loc[~agecond].Age) 
# pandas will use ~ as negation operator  
rTrain_Age_Mode # we only need the mode, let's extract it
rTrain_Age_Mode.mode # almost there, just need the value from inside the array
# finding top frequency 
rTrain_Age_Mode.mode[0] # finally...
rTrain_Age_Mode = rTrain_Age_Mode.mode[0] # yep, we keep only the relevant value
rTrain_Age_Mode

# need to save this value for reference  24.0
cleanupValues['Age.null.imputation'] = rTrain_Age_Mode
"""

newData.loc[pd.isnull(newData.Age), 'Age'] = cleanupValues['Age.null.imputation']

#%% resuing the code from Lecture 02... 

agecond = pd.isnull(newData.Age)
# create variable that keeps truack of which values of Age were imputed 
newData = ['Age_imputed'] = 0
newData.loc[agecond, 'Age_imputed'] = 1

# assume, do not check, that the distribution of missings in Age is similar to the one in the training data 
# let's do a cross-tabulation to make sure we flagged exactly the missing Age records 
# actual imputation 
newData.loc[agecond, 'Age'] = cleanupValues['Age.null.imputation']
#??????????? NoneType object is not subscriptable 
# Have to change it to def function - cleanupRawData(newData, cleanupValues = cleanupValues)

#%% Impute Embarked 
newData.loc[pd.isnull(newData.Embarked), 'Embarked'] = cleanupValues['Embarked.null.imputation']

"""
# now impute... but let's keep track of where we imputed, eh?
rTrain['Age_imputed'] = 0
rTrain.loc[agecond, 'Age_imputed'] = 1
# let's do a cross-tabulation to make sure we flagged exactly the missing Age records
pd.crosstab(rTrain['Age_imputed'], agecond)
# now we're ready for imputation
rTrain.loc[agecond, 'Age'] = rTrain_Age_Mode
# check with a histogram
sns.distplot(rTrain.Age)

#%%

# how about Embarked?
rTrain.loc[pd.isnull(rTrain.Embarked)]
rTrain.loc[pd.isnull(rTrain.Embarked)].drop(columns={'Survived','Pclass','Embarked','Fare','Age_imputed'})
# hmm - same ticket, seem like mother/daughter or so, same cabin, same ticket price
pd.crosstab(rTrain['Embarked'],rTrain['Pclass'])
# most first class passengers embarked in S - let's assign them there (other choices available)
rTrain.loc[pd.isnull(rTrain.Embarked), 'Embarked'] = 'S'

# also, the PassengerID is unique (and random) to passengers, so should not be related to survival outcome
rTrain = rTrain.drop(columns={'PassengerId', 'Cabin'})

# check...
rTrain.dtypes
pd.isnull(rTrain).astype(int).aggregate(sum)
rTrain.Ticket.value_counts()
"""
#%% drop ticket and name as well...
newData = newData.drop(columns={'Ticket', 'Name'}) #@@ should check that these coluns exist? 

# drop ticket and name as well... 
newData = newData.drop(columns = {'PassengerID', 'cabin'})

#%% save cleaned training data

newDataCelan = cleanupRawData(newData, cleanupValues = cleanupValues)
# actually apply it to the train data as well 
#cleanupRawData(rTrain, cleanupValues = cleanupValues).to_pickle(trainCleanFIle)