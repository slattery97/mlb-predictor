# MLB Linear Regression

# Importing the dataset
dataset = read.csv('WSox_OverUnder_Data.csv')
dataset = na.omit(dataset)

# Encoding categorical data
dataset$State = factor(dataset$HomeAway,
                       levels = c('Away', 'Home'),
                       labels = c(1, 2))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$TotalRuns, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = training_set[2],
                         y = training_set$TotalRuns,
                         ntree = 500)

# Show summary of regressor
summary(regressor)

# Predicting a new result
y_pred = predict(regressor, data.frame(SIERA = 9.5))

# Compare actual vs predicted run totals
datasetTraining = data.frame(ActualRuns = training_set$TotalRuns, 
                      PredictedRuns = predict(regressor, newdata = training_set),
                      RunLine = training_set$OverUnder)

datasetTesting = data.frame(ActualRuns = test_set$TotalRuns, 
                             PredictedRuns = predict(regressor, newdata = test_set),
                             RunLine = test_set$OverUnder)

# Export prediction results
write.csv(datasetTraining, file = 'mlb-svr-results-train.csv')
write.csv(datasetTesting, file = 'mlb-svr-results-test.csv')


# Visualizing the Random Forest results
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$SIERA), max(dataset$SIERA), 0.01)
ggplot() +
  geom_point(aes(x = dataset$SIERA, y = dataset$TotalRuns),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(SIERA = x_grid))),
            colour = 'blue') +
  ggtitle('MLB Total Runs Prediction (Random Forest)') +
  xlab('Starting Pitchers Total SIERA') +
  ylab('Total Runs')






