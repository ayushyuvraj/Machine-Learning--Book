# Importing the dataset
dataset = read.csv('mushrooms.csv')
dataset$veil_type = NULL

#------------------------------------------------------------------------------
# Building model2 on the entire dataset and predicting on the entire dataset

# making the model and predicting
classifier = rpart(formula = type ~ .,
                    data = dataset)
y_pred = predict(classifier, dataset[-1], type = 'class')

# Confusion matrix
cm= table(dataset$type, y_pred)

# Calculating the accuracy of the model2
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])

# Plotting the decision tree
plot(classifier)
text(classifier)

#------------------------------------------------------------------------------

# Building model1 after splitting the dataset

# Splitting the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$type, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#------------------------------------------------------------------------------

# making the model and predicting using rpart
classifier1 = rpart(formula = type ~ .,
                   data = training_set)
y_pred1 = predict(classifier1, test_set[-1], type = 'class')

# Confusion matrix
cm1= table(test_set$type, y_pred1)

# Calculating the accuracy of the model1
accuracy1 = (cm1[1,1]+cm1[2,2])/(cm1[1,1]+cm1[2,2]+cm1[1,2]+cm1[2,1])

# Plotting the decision tree
plot(classifier1)
text(classifier1)

#------------------------------------------------------------------------------

# making the model and predicting using'tree'
classifier2 = tree(formula = type ~ .,
                    data = training_set)
y_pred2 = predict(classifier2, test_set[-1], type = 'class')

# Confusion matrix
cm2= table(test_set$type, y_pred2)

# Calculating the accuracy of the model1
accuracy2 = (cm2[1,1]+cm2[2,2])/(cm2[1,1]+cm2[2,2]+cm2[1,2]+cm2[2,1])

# Plotting the decision tree
plot(classifier2)
text(classifier2)