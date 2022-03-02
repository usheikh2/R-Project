# Install Packages
library(caret)
library(dplyr)
library(randomForest)
# Read in CSV file
spotify = read.csv(file = 'spotify_dataset.csv')
head(spotify)

# Identify missing values 
totalmissing = sum(is.na(spotify))
totalmissing

totalcells = prod(dim(spotify))
totalcells

pctmissing = (totalmissing * 100) / totalcells
pctmissing

# "Genre" excluded
X = spotify[ , c("Artist", "Artist.Followers", "Energy",
              "Streams", "Weeks.Charted", "Popularity", 
              "Danceability", "Acousticness", "Loudness",
              "Speechiness", "Tempo", "Valence")]
Y = spotify["Highest.Charting.Position"]

# Training set & test set 
set.seed(233)

training = createDataPartition( spotify$Highest.Charting.Position, p = 0.8, list = FALSE)
XTrain = X[training,]
XTest = X[-training,]

YTrain = Y[training,]
YTest = Y[-training,]

# Set up training data
trainData = data.frame( x= XTrain, y= YTrain)

# Fit random forest 
rfspot = randomForest( y ~ ., data = trainData, ntree = 500)

# Predict 
rf_yHat = predict(rfspot,newdata=data.frame(x= ))


# Performance eval
rfPR = postResample(pred=rf_yHat, obs=solTestY)
rfPR