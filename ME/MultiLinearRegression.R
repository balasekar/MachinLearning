# Multi Linear Regression - Consumption = K1 Temperature + K2 Bedrooms + K3 Month + K4 Min Temp 

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Consumption, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multi Linear Regression to the dataset
multi_lin_reg = lm(formula = Consumption ~ ., data = training_set)
summary(multi_lin_reg)

# Removing minTemp, Month variables from the equation because of less significance
multi_lin_reg = lm(formula = Consumption ~ Temp + Bedrooms, data = training_set)
summary(multi_lin_reg)

# Predicting the Test set results
y_pred = predict(multi_lin_reg, newdata = test_set)

# Visualising the Multi Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Temp, y = dataset$Consumption),
             colour = 'red') +
  geom_line(aes(x = dataset$Temp, y = predict(multi_lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Multi Linear Regression - Consumption Prediction Model') +
  xlab('Temperature (F)') +
  ylab('Consumption')

# Visualising the Multi Linear Regression results
ggplot() +
  geom_point(aes(x = test_set$Temp, y = test_set$Consumption),
             colour = 'red') +
  geom_line(aes(x = test_set$Temp, y = predict(multi_lin_reg, newdata = test_set)),
            colour = 'blue') +
  ggtitle('Multi Linear Regression - Consumption Prediction Model') +
  xlab('Temperature (F)') +
  ylab('Consumption')

# Predicting a new result with Multi Linear Regression
predict(multi_lin_reg, data.frame(Temp = 51, Bedrooms = 3))