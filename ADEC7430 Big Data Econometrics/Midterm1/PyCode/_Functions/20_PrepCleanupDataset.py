# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 08:50:01 2019

@author: RV
Purpose: Build function to prepare train data for model estimation
Inputs:  DataFrame of raw data imported
Outputs: DataFrame (saved) of data cleaned/prepared
         Cleaning function saved as an object - ready to be applied to a test dataset
"""

#%% what data will this be applied to?
rTrain = pd.read_pickle(rTrainFile)
#newData = rTrain.copy() # ONLY USE THE FIRST TIME / TRAINING ALGO
# in practice, this is not a copy of Train, but a new dataset

cleanupValues = pd.read_pickle(cleanupValuesFilePath)

#%% calculate age modes for newData
def calculateMode(df, cleanupValues = None): 
    agecond = pd.isnull(df.Age)
    Age_Mode = scipy.stats.mode(df.loc[~agecond].Age)
    Age_Mode = Age_Mode.mode[0]
    
    return(Age_Mode)

#cleanupValues['Age.null.imputation'] = 
#calculateMode(newData)

def calculateFareMode(df, cleanupValues = None): 
    xcond = pd.isnull(df.Fare)
    Fare_Mode = scipy.stats.mode(df.loc[~xcond].Fare)
    Fare_Mode = Fare_Mode.mode[0]
    
    return(Fare_Mode)
#cleanupValues['Fare.null.imputation'] = 
#calculateFareMode(newData)

#%% package into a function
def cleanupRawData(newData, cleanupValues = None):
    # reusing the code from Lecture 02...
    
    agecond = pd.isnull(newData.Age)
    # create variable that keeps track of which values of Age were imputed
    newData['Age_imputed'] = 0
    newData.loc[agecond, 'Age_imputed'] = 1
    # assume, do not check, that the distribution of missings in Age is similar to the one in the training data
    
    # actual imputation
    newData.loc[agecond, 'Age'] = cleanupValues['Age.null.imputation']
    
    # Impute Embarked
    newData.loc[pd.isnull(newData.Embarked), 'Embarked'] = cleanupValues['Embarked.null.imputation']
    
    # Impute Fare 
    xcond = pd.isnull(newData.Fare)
    newData.loc[xcond, 'Fare'] = cleanupValues['Fare.null.imputation']
    
    # drop ticket and name as well...
    newData = newData.drop(columns={'Ticket', 'Name'}) #@@ should check that these columns exist
    #==============================================================================
    # drop PassengerId and Cabin as well...
    newData = newData.drop(columns={'PassengerId', 'Cabin'})

    return(newData)

# save cleaned training data

newData = cleanupRawData(newData, cleanupValues =cleanupValues)

# actually apply it to the train data as well
#cleanupRawData(rTrain, cleanupValues =cleanupValues).to_pickle(trainCleanFile)

#%% create copy with dummies from character variables, as well
#@@ package into function !!

def createDummies(data, outfileNamePrefix = 'trainWDummies'):
#data = newData.copy()
    data = data.drop(columns = 'Age_imputed')
    
    cat_vars=['Sex','Embarked']
    for var in cat_vars:
    #   cat_list='var'+'_'+var
        cat_list = pd.get_dummies(data[var], prefix=var) 
        # this creates dummy variables from a categorical/character variable
        data1=data.join(cat_list)
        data=data1
    
    data_vars=data.columns.values.tolist()
    to_keep=[i for i in data_vars if i not in cat_vars]
    data = data[to_keep].copy()
    
    # separate predictors from outcome prior to fitting the model
    X = data.drop(columns={'Survived'})
    y = data['Survived']
        
    X.to_pickle(os.path.join(savedDataFld, outfileNamePrefix + '_Predictors.pkl'))
    y.to_pickle(os.path.join(savedDataFld, outfileNamePrefix + '_Outcome.pkl'))
createDummies(newData, 'mytrial_20190301')
createDummies(rawData, '')

#%% running train2 models and store X values 
newData = train2.copy() # in practice, this is not a copy of Train, but a new dataset
newData = cleanupRawData(newData, cleanupValues = cleanupValues)

data = newData.copy()
data = data.drop(columns = 'Age_imputed')
    
cat_vars=['Sex','Embarked']
for var in cat_vars:
    #   cat_list='var'+'_'+var
    cat_list = pd.get_dummies(data[var], prefix=var) # this creates dummy variables from a categorical/character variable
    data1=data.join(cat_list)
    data=data1
    
data_vars=data.columns.values.tolist()
to_keep=[i for i in data_vars if i not in cat_vars]
data = data[to_keep].copy()

train2_X = data.drop(columns={'Survived'})
train2_y = data['Survived']
train2_X.to_pickle(os.path.join(savedDataFld, 'testPredictorsDummies.pkl'))
train2_y.to_pickle(os.path.join(savedDataFld, 'testOutcome.pkl'))

#%% running rawTest dataset and store test_X values 
newData = rawTest.copy()
# acutal imputation of Fare

newData = cleanupRawData(newData, cleanupValues = cleanupValues)

data = newData.copy()
data = data.drop(columns = 'Age_imputed')
    
cat_vars=['Sex','Embarked']
for var in cat_vars:
    #   cat_list='var'+'_'+var
    cat_list = pd.get_dummies(data[var], prefix=var) # this creates dummy variables from a categorical/character variable
    data1=data.join(cat_list)
    data=data1
    
data_vars=data.columns.values.tolist()
to_keep=[i for i in data_vars if i not in cat_vars]
X_test = data[to_keep].copy()

X_test.to_pickle(os.path.join(savedDataFld, 'actualPredictorsDummies.pkl'))
