setwd("~/DAEN Info/OR568/OR568project") #set directory to folder containing data
#install.packages
library(caret)
library(readr)
library(corrplot)

spotify_dataset <- read_csv(file = "spotify_dataset.csv")

summary(spotify_dataset)

colnames(spotify_dataset)

streams <- spotify_dataset$Streams


#Remove rows with na

SPFY <- na.omit(spotify_dataset)

## Copied hw.R file from a previous class STAT515 for pretty graphs

#source("C:/Users/jeani/OneDrive/Documents/DAEN Schoolwork/STAT515 FallII 2020/Module 1/hw.R")

ggplot(SPFY, aes(x = Streams, y = SPFY$`Highest Charting Position`)) +
  geom_point(
    shape = 21,
    fill = "blue",
    color = "black",
    size = 3
  ) +
  labs(x = "Streams",
       title = "Highest Charting Position by Streams") + hw
# This graph might be more visible without the very high stream values
# it seems unusual that there are songs with very high streams but are not high on the chart position

ggplot(SPFY, aes(x = `Artist Followers`, y = `Highest Charting Position`)) +
  geom_point(
    shape = 21,
    fill = "blue",
    color = "black",
    size = 3
  ) +
  labs(x = "Artist Followers",
       title = "Highest Charting Position by Streams") + hw


#remove non-numeric columns and index column
numSpotify_dataset <- na.omit(spotify_dataset[,-c(1,4,5,7,9,10,11,12,23)])

Predictors <- numSpotify_dataset[,-1]


TransPredictors <- as.data.frame(scale(Predictors), center = TRUE, scale = TRUE)
HiCharPosit <- na.omit(numSpotify_dataset$`Highest Charting Position`)

summary(HiCharPosit)

#split the data
#start with a partition designation
set.seed(1)
trainingRows <- createDataPartition(HiCharPosit, p = 0.8, list = FALSE)


#subset into training and test for predictors and highest charting position
trainPredictors <- TransPredictors[trainingRows, ]
trainHiCharPosit <- HiCharPosit[trainingRows]
testPredictors <- TransPredictors[-trainingRows,]
testHiCharPosit <- HiCharPosit[-trainingRows]

### Copied code from OR568 course reference R files

featurePlot(trainPredictors, trainHiCharPosit, 
            between = list(x = 1, y = 1), type = c("g", "p", "smooth"), labels = rep("", 2))

# Figure 6.4  PCA 
solPCA <- prcomp(trainPredictors, center = TRUE, scale. = TRUE)
percentVariancePCA = solPCA$sd^2/sum(solPCA$sd^2)*100
plot(percentVariancePCA, xlab="Components", ylab="Percentage of Total Variance",
     type="l", main="Scree Plot of PCA")

#correlation plot for predictors
corrplot(cor(trainPredictors), order = "hclust", tl.cex = .8)


### Section 6.2 Linear Regression

## Use simple linear regression with all predictors 

lmFitAllPredictors = lm(trainHiCharPosit ~ . , data = trainPredictors)
summary(lmFitAllPredictors)

lmFitStreams = lm(trainHiCharPosit ~trainPredictors$Streams, data = trainPredictors)
summary(lmFitStreams)

lmFitNumTimesChar = lm(trainHiCharPosit ~trainPredictors$'Number of Times Charted', 
                       data = trainPredictors)
summary(lmFitNumTimesChar)
#Rsquared values are very bad.

#Estimate the test set performance
lmPred1 <- predict(lmFitAllPredictors, testPredictors)
head(lmPred1)

#Resampling estimate of performance
ctrl <- trainControl(method = "cv", number = 10)
set.seed(1)
lmFit1 <- train(x = trainPredictors, y = trainHiCharPosit, method = "lm", trControl = ctrl)
lmFit1
#RMSE 48.24899, Rsquared 0.3259047, MAE 40.3786

xyplot(trainHiCharPosit~ predict(lmFit1), type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

xyplot(resid(lmFit1)~ predict(lmFit1), type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")
# Try fitting a logistic Regression Model
logisticReg <- train(x = trainPredictors, y = trainHiCharPosit, method = "glm", trControl = ctrl)
logisticReg

# RMSE 48.24561, Rsquared 0.3143689, MAE 40.41483