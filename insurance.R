# Importing the dataset
dataset = read.csv('insurance.csv')

# Adding new variables
dataset$age2 = dataset$age^2

# Converting continuous to categorical variable
dataset$bmi = ifelse(dataset$bmi>=30,1,0)

# Splitting the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$charges, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Building modified regressor and predicting model using linear regression
regressor = lm(formula = charges ~ age+age2+children+bmi+sex+bmi*smoker+region,
               data = training_set)
y_pred = predict(regressor, newdata = test_set[-7])
