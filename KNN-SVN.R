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
#Pre-process - Min/Max Normalization
Data_Norm <- function(x) { ((x - min(x)) / (max(x)- min(x)))}
dfSP_Norm <-  as.data.frame(lapply(dfSP[,-1], Data_Norm))
head(dfSP_Norm)

#Split data into training and test sets
#Create training set and test set
dfSP_Train <- dfSP_Norm[1:1159,]
dfSP_Test <- dfSP_Norm[1160:1545,]

#KNN Model
knnModel <- train(x = dfSP_Train, y = dfSP_Train$Liveness,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 10)

#Plot KNN model to view number of neighbors
plot(knnModel, main = "KNN Model")
knnModel
#Predict
knnPred <- predict(knnModel, newdata = dfSP_Test)
knnPred

# Test Set Performance Values
postResample(pred = knnPred, obs = dfSP_Test$Liveness)


#--------------------------- SVM ---------------------------#
svmRTuned <- train(x = dfSP_Train, y = dfSP_Train$Liveness,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneLength = 14,
                   trControl = trainControl(method = "cv"))
svmRTuned
svmRTuned$finalModel
plot(svmRTuned, xlim=c(0,15), main = "SVM Tuned Model")
