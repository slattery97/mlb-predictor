# MLB Linear Regression

# Importing the dataset
dataset = read.csv('WSox_OverUnder_Data.csv')

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

# Fitting Multiple Linear Regression to the Training set
#regressor = lm(formula = Profit ~ .,
#               data = training_set)

# Predicting the Test set results
#y_pred = predict(regressor, newdata = test_set)

# Build the optimal model using Backward Elimination (Significance level = 0.05)
# Remove each variable with p-value below SL and re-run regression until only
# statistically significant variables remain
regressor = lm(formula = TotalRuns ~ SIERA,
               data = dataset)
# Show summary of regressor
summary(regressor)

# Compare actual vs predicted run totals
datasetTraining = data.frame(ActualRuns = training_set$TotalRuns, 
                      PredictedRuns = predict(regressor, newdata = training_set),
                      RunLine = training_set$OverUnder)

datasetTesting = data.frame(ActualRuns = test_set$TotalRuns, 
                             PredictedRuns = predict(regressor, newdata = test_set),
                             RunLine = test_set$OverUnder)

# Export prediction results
write.csv(datasetTraining, file = 'mlb-prediction-results-train.csv')
write.csv(datasetTesting, file = 'mlb-prediction-results-test.csv')


# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$SIERA, y = training_set$TotalRuns),
             colour = 'red') +
  geom_line(aes(x = training_set$SIERA, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Combined SIERA vs Total Runs (Training set)') +
  xlab('Combined SIERA') +
  ylab('Total Runs')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$SIERA, y = test_set$TotalRuns),
             colour = 'red') +
  geom_line(aes(x = training_set$SIERA, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Combined SIERA vs Total Runs (Test set)') +
  xlab('Combined SIERA') +
  ylab('Total Runs')






