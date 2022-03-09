# Install Packages
library(caret)
library(dplyr)
library(randomForest)
library(readr)

# Read in CSV file
spotify = read.csv(file = "spotify_dataset.csv")

# Convert streams to numeric
spotify$Streams = gsub(",", "",as.character(spotify$Streams))
spotify$Streams = as.numeric(spotify$Streams)

# Identify missing values - less than 1% is missing
totalmissing = sum(is.na(spotify))
totalmissing

totalcells = prod(dim(spotify))
totalcells

pctmissing = (totalmissing*100) / totalcells
pctmissing
spotify_complete = na.omit(spotify)
spotify_complete

# Genre and Weeks Charted excluded from original plans  
X = spotify_complete[ , c("Artist.Followers", "Energy", "Loudness",
                         "Streams", "Popularity", "Danceability", 
                         "Acousticness", "Speechiness",
                         "Tempo", "Valence", "Duration..ms.", "Liveness")]
spotify_complete$Highest.Charting.Position = as.factor(spotify_complete$Highest.Charting.Position)
str(spotify_complete)
Y = spotify_complete["Highest.Charting.Position"]

# Training set and test set data split
set.seed(200)
training = createDataPartition( spotify_complete$Highest.Charting.Position, 
                                p = 0.8, list = FALSE)
XTrain = X[training, ]
XTest = X[-training, ]

YTrain = Y[training, ]
YTest = Y[-training, ]

# Set up training data 
trainData = data.frame( x = XTrain, y = YTrain )

# Fit RF

y = unlist(Y)
rfspot = randomForest( y ~ ., data = trainData, ntree = 17, corr.bias = TRUE )
rfspot

# Predict

rf_y = predict(rfspot, newdata = data.frame (x = XTrain))
rf_y

# Performance eval
rfPR = postResample( pred = rf_y, obs = YTest)
rfPR

# Variable Importance Plot 
varImpPlot(rfspot) 
