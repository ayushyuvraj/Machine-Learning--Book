# NaiveBayes

# Data Preprocessing
dataset_original = read.csv('sms_spam.csv', stringsAsFactors = FALSE)
names(dataset_original)[1]='realtype'

# Encoding the target feature as factor
dataset_original$realtype = factor(dataset_original$realtype,
                                   levels = c('ham','spam'),
                                   labels = c('1','0'))
# Creating corpus
library(tm)
corpus = VCorpus(VectorSource(dataset_original$text))

# Preparation for building prediction model
# Creating frequency matrix
dtm = DocumentTermMatrix(corpus,list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  stripWhitespace = TRUE,
  stemming = TRUE
))
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
colnames(dataset) = make.names(colnames(dataset))
dataset$realtype = dataset_original$realtype

# Our data is now ready to build prediction model
# building NaiveBayes Prediction model


# Splitting the data
library(caTools)
set.seed(123)
split = sample.split(dataset$realtype, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Building a model using randomForest
# Making a classifier
library(randomForest)
classifier = randomForest(x = training_set[-1211],
                          y = training_set$realtype,
                          ntree = 10)

# predictinig the results
y_pred = predict(classifier, newdata = test_set[,-1211])

# Building confusion matrix
cm = table(test_set$realtype,y_pred)