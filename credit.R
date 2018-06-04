# Importing the dataset
dataset = read.csv('credit.csv')

# Converting misclassified columns to factors
dataset$existing_credits = as.factor(dataset$existing_credits)
dataset$default = as.factor(dataset$default)
dataset$dependents = as.factor(dataset$dependents)

# Making the dataset look numeric - NOT IMPORTANT
dataset$credit_history = factor(dataset$credit_history, labels = 1:5)
dataset$checking_balance = factor(dataset$checking_balance, labels = 1:4)
dataset$purpose = factor(dataset$purpose, labels = 1:10)
dataset$savings_balance = factor(dataset$savings_balance, labels = 1:5)
dataset$employment_length = factor(dataset$employment_length, labels = 1:5)
dataset$personal_status = factor(dataset$personal_status, labels = 1:4)
dataset$other_debtors = factor(dataset$other_debtors, labels = 1:3)
dataset$property = factor(dataset$property, labels = 1:4)
dataset$installment_plan = factor(dataset$installment_plan, labels = 1:3)
dataset$housing = factor(dataset$housing, labels = 1:3)
dataset$telephone = factor(dataset$telephone, labels = 1:2)
dataset$foreign_worker = factor(dataset$foreign_worker, labels = 1:2)
dataset$job = factor(dataset$job, labels = 1:4)

# Splitting the data
library(caTools)
set.seed(123)
split = sample.split(dataset$default, SplitRatio = 0.8)
training_set = subset(dataset, split ==TRUE)
test_set = subset(dataset, split ==FALSE)

#--------------------------------------------------------------------------

# building tree using 'rpart'
# Building classifier
library(rpart)
classifier1 = rpart(formula = default ~ .,
                   data = training_set)
# Making the prediction model
y_pred1 = predict(classifier1 , newdata = test_set[-17], type = 'class')
# Making the confusion matrix
cm1 = table(test_set$default, y_pred1)
#Calculating accuracy
accuracy1 = (cm1[1,1]+cm1[2,2])/(cm1[1,1]+cm1[1,2]+cm1[2,1]+cm1[2,2])
# Plotting the tree
plot(classifier1)
text(classifier1,pretty = 0, cex = 0.7)


#-------------------------------------------------------------------------


#building tree using 'tree'
# Building classifier
library(tree)
classifier2 = tree(formula = default ~ .,
                   data = training_set)
# Making the prediction model
y_pred2 = predict(classifier2 , newdata = test_set[-17], type = 'class')
# Making the confusion matrix
cm2 = table(test_set$default, y_pred2)
#Calculating accuracy
accuracy2 = (cm2[1,1]+cm2[2,2])/(cm2[1,1]+cm2[1,2]+cm2[2,1]+cm2[2,2])
# Plotting the tree
plot(classifier2)
text(classifier2,pretty = 0, cex = 0.7)


#-------------------------------------------------------------------------

#building tree using  Random Forest
# Building classifier
library(randomForest)
classifier3 = randomForest(x = training_set[-17],
                           y = training_set$default,
                           ntree = 100)
# Making the prediction model
y_pred3 = predict(classifier3 , newdata = test_set[,-17])
# Making the confusion matrix
cm3 = table(test_set$default, y_pred3)
#Calculating accuracy
accuracy3 = (cm3[1,1]+cm3[2,2])/(cm3[1,1]+cm3[1,2]+cm3[2,1]+cm3[2,2])
# Plotting the tree
plot(classifier3)
