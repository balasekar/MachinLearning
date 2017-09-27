# Simple Linear Regression - Consumption = K Temperature

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Consumption, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the dataset
lin_reg = lm(formula = Consumption ~ Temp, data = training_set)
summary(lin_reg)

# Predicting the Test set results
y_pred = predict(lin_reg, newdata = test_set)
print(y_pred);

# Visualising the Simple Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Temp, y = dataset$Consumption),
             colour = 'red') +
  geom_line(aes(x = dataset$Temp, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Simple Linear Regression - Consumption Prediction Model') +
  xlab('Temperature (F)') +
  ylab('Consumption')


# Visualising the Simple Linear Regression Test results
ggplot() +
  geom_point(aes(x = test_set$Temp, y = test_set$Consumption),
             colour = 'red') +
  geom_line(aes(x = test_set$Temp, y = predict(lin_reg, newdata = test_set)),
            colour = 'blue') +
  ggtitle('Simple Linear Regression - Consumption Prediction Model') +
  xlab('Temperature (F)') +
  ylab('Consumption')

predict(lin_reg, data.frame(Temp = 51))