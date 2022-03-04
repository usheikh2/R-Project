# Install Packages
library(AppliedPredictiveModeling)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
# Read in CSV file
spotify1 <- read.csv("spotify_dataset.csv")

#Remove null values
spotify1 <- spotify1[complete.cases(spotify1),]

#Remove unused columns
spotify <- spotify1[, -c(1,3,4,5,7,9,10,11,12,21,23)]

#Turn convert streams column from chr to numeric
spotify$Streams <- gsub(",", "",as.character(spotify$Streams))
spotify$Streams <- as.numeric(spotify$Streams)

# Split data into training/testing
trainingRows <- createDataPartition(spotify[,1],p = .80, list= FALSE)
spotifyTest <- spotify[trainingRows,]
spotifyTrain <- spotify[-trainingRows,]

## Fit First Tree with all Predictors
fit <- rpart(Highest.Charting.Position ~ ., method = "anova", data = spotifyTrain, control=rpart.control(cp=0.01,maxdepth=30), preProc = c("center", "scale"))
rpart.plot(fit, main="Classification Tree Model 1")

# Test Model 
pred <- predict(fit, spotifyTest) 
postResample(pred = pred, obs = spotifyTest$Highest.Charting.Position)

# Plot predictor importance for future models
df <- data.frame(imp = fit$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("Variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()

#Prune Model
pfit<- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, main="Classification Tree Model 1 Post Prune")
pred <- predict(pfit, spotifyTest) 
postResample(pred = pred, obs = spotifyTest$Highest.Charting.Position)
pfit


##Fit model with only the best predictor(streams)

fit2 <- rpart(Highest.Charting.Position ~ Streams, method = "anova", data = spotifyTrain, control=rpart.control(cp=0.01,maxdepth=30))
fit2 <- prune(fit2, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(fit2, main="Classification Tree Model 2")

#Test model 
pred <- predict(fit2, spotifyTest) 
postResample(pred = pred, obs = spotifyTest$Highest.Charting.Position)

##Fit Model with top 3 Predictors

fit3 <- rpart(Highest.Charting.Position ~ Streams + Artist.Followers + Popularity, method = "anova", data = spotifyTrain, control=rpart.control(cp=0.01,maxdepth=30))
fit3 <- prune(fit3, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(fit3, main="Classification Tree Model 3")

# Test Model 
pred <- predict(fit3, spotifyTest) 
postResample(pred = pred, obs = spotifyTest$Highest.Charting.Position)

