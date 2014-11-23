##########################################################################################
# Title:  Interactive session for Assignment 'Write-up' for Practical Learning Machine
# Date:   November 2014
# Author: Mib
#
# This interactive script builds models (trainedModel) learned from a training set 
# of sensor data attached to athletes and their equipment whilst performing a particular 
# weight-lifting exercise (biceps curl)
#
# Summary of algorithm
# 
# 1. Open caret library, from which the learning algorithm (train) is used
# 2. Sort the raw training data into random order to remove any spurious 
#    correlations between data an the initial set ordering 
# 3. Split the data into a training set and a test set using makeDataPartition
# 4. Create list of predictors to be used in the learning process. (for instance:
#    current setting is for variables 8:10 ie. belt roll, pitch and yaw)
# 5. Train the model using 'train with the selected training method
# 7. Use 'predict' function to predict the classe of test data in the Trained model
# 6. Display the results in a confusion matrix
##########################################################################################

library(caret)
set.seed(223)
# read training and test files
rawTraining <- read.csv("pml-training.csv")

# sort the raw training data into random order to remove any spurious 
# correlations between data an the initial set ordering 

t3=cbind(rawTraining,runif(1:nrow(rawTraining)))
t4 = order(t3[,ncol(t3)],na.last = FALSE,decreasing = TRUE)
t5 = t3[t4,1:160]

# create data partition to get training and testing sets in proportion p
partition = 0.6
inTrain = createDataPartition(y=t5$classe,p=partition,list = FALSE)
pmlTraining = rawTraining[inTrain,]
pmlTesting = rawTraining[-inTrain,]
 
# build data subsets for combinations of variables (in this case Belt roll, pitch and yaw)
numericVar = c(8:10,37:39,46:48,60:62,84:86,113:115,122:124,151:153)
factVar = c(2,12:17,46:48,87:92,95,98,125:130,133,136,139)
currentVar = c(numericVar, factVar,160)
currentVar = c(8:10,160)
# restrict sets to only those columns specified in the currentVar list
currentTraining = pmlTraining[,currentVar]
currentTesting  = pmlTesting[,currentVar]

# choose training method, then build a model
method = "C5.0Tree"
trainedModel = train(classe~.,data=currentTraining,method=method)

# predict classe from test set using the trained model
prediction = predict(trainedModel,currentTesting)

# create confusion matrix and display results
confusionMatrix(currentTesting$classe, prediction)
featurePlot(x=rawTraining[,c(8,9,10)], y = rawTraining$classe,plot= "pairs")
qplot(rawTraining[,8], rawTraining[,9], colour = rawTraining$classe, data = rawTraining[,])

# End