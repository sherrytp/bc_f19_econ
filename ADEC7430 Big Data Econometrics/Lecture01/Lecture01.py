# %% Setup
import os
from collections import Counter
from scipy.stats import mode

projFld = "C:/Users/RV/Documents/Teaching/2019_01_Spring/ADEC7430_Spring2019/Lecture02"
codeFld = os.path.join(projFld, "PyCode")
fnsFld = os.path.join(codeFld, "_Functions")
outputFld = os.path.join(projFld, "Output")
rawDataFld = os.path.join(projFld, "RawData")
savedDataFld = os.path.join(projFld, "SavedData")

# %% load some functions
fnList = ["fn_logMyInfo"]  # this is a list
for fn in fnList:
    exec (open(os.path.join(fnsFld, fn + ".py")).read())
# Explain: name, extension name, read+write
# create also a file where we will log some data
logf = os.path.join(projFld, "logfile.csv")

# test writing to log
from fn_logMyInfo import fn_logMyInfo

fn_logMyInfo("test writing to log", useConsole=True, useFile=logf)

# %% mode of a list of numbers
testList = [1, 2, 3, 4, 1, 2, 3, 1, 2, 1]

tcnts = Counter(testList)
tcnts.most_common()
tcnts.most_common(1)
tcnts.most_common(2)

# does this work on strings?
testList2 = [str(i) for i in testList]
tcnts2 = Counter(testList2)
tcnts2.most_common()  # seem so

# another way:
mode(testList)
mode(testList2)

from statistics import mode
mode(testList)
mode(testList2)
# try to tease the object above...
x = mode(testList)
type(x)
