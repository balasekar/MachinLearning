# Polynomial Regression

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Consumption, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the dataset
lin_reg = lm(formula = Consumption ~ .,
             data = training_set)
summary(lin_reg)

lin_reg = lm(formula = Consumption ~ Temp + Bedrooms,
             data = training_set)
summary(lin_reg)


# Predicting the Test set results
y_pred = predict(lin_reg, newdata = test_set)

summary(y_pred)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Temp, y = dataset$Consumption),
             colour = 'red') +
  geom_line(aes(x = dataset$Temp, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Linear Regression - Consumption Prediction Model') +
  xlab('Temperature') +
  ylab('Consumption')

# Fitting Polynomial Regression to the dataset
dataset$Temp2 = dataset$Temp^2
dataset$Temp3 = dataset$Temp^3
poly_reg = polym(formula = Consumption ~  Temp + Temp2 + Temp3 ,
              data = dataset)


# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Temp, y = dataset$Consumption),
             colour = 'red') +
  geom_line(aes(x = dataset$Temp, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Polynomial Regression - Consumption Prediction Model') +
  xlab('Temperature') +
  ylab('Consumption')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Temp), max(dataset$Temp), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Temp, y = dataset$Consumption),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Temp = x_grid,
                                                             Temp2 = x_grid^2,
                                                             Temp3 = x_grid^3))),
            colour = 'blue') +
  ggtitle('Polynomial Regression - Consumption Prediction Model') +
  xlab('Temperature') +
  ylab('Consumption')

# Predicting a new result with Linear Regression
predict(lin_reg, data.frame(Temp = 51, Bedrooms = 3))

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))