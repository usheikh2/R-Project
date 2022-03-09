library(readr)
library(AppliedPredictiveModeling)
library(caret)
library(class)
library(data.table)

#Read Spotify Dataset
spotify_dataset <- read.csv("spotify_dataset.csv", header=TRUE, sep=",")
View(spotify_dataset)

#Remove null values and character values
db = spotify_dataset[complete.cases(spotify_dataset),]
dfSP <- db[,-c(1:13,20,21,23)]

#----------------------- KNN Model -----------------------#
#Pre-process - Min/Max Normalization (optional - provides a little higher R^2 value) 
Data_Norm <- function(x) { ((x - min(x)) / (max(x)- min(x)))}
dfSP_Norm <-  as.data.frame(lapply(dfSP[,-1], Data_Norm))
head(dfSP_Norm)

#Split data into training and test sets
#Create training set and test set
dfSP_Train <- dfSP[1:1159,]
dfSP_Test <- dfSP[1160:1545,]

#KNN Model - centered and scaled & 10 k-fold
set.seed(100)
knnModel <- train(x = dfSP_Train, y = dfSP_Train$Liveness,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  trControl = trainControl(method = "cv"))

#Plot KNN model to view number of neighbors
plot(knnModel, main = "KNN Model")
knnModel

#The predict() function is used to predict the values based on the input data
knnPred <- predict(knnModel, newdata = dfSP_Test)
knnPred

## The postResample() function is  used to get the test set performance 
postResample(pred = knnPred, obs = dfSP_Test$Liveness)


#--------------------------- SVM ---------------------------#

#SVM Model - centered and scaled & 10 k-fold
svmRTuned <- train(x = dfSP_Train, y = dfSP_Train$Liveness,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneLength = 14,
                   trControl = trainControl(method = "cv"))

#The model used 198 training set data points as support vectors 
svmRTuned

#finalModel contains the model created by the ksvm function
svmRTuned$finalModel

#Plot SVM Model
plot(svmRTuned, xlim=c(0,15), main = "SVM Tuned Model")
