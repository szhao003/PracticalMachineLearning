#Loading libraries
library(caret)
library(knitr)
library(randomForest)

#Read the csv file and transfer NULL to NA.
training <- read.csv("F:/Data Science/Data Science/CourseraClasses/Data Scientist Specialization MOOC/Practical Machine Learning/Homework 1/pml-training.csv", na.string = c("", "NA"))
testing <- read.csv("F:/Data Science/Data Science/CourseraClasses/Data Scientist Specialization MOOC/Practical Machine Learning/Homework 1/pml-testing.csv", na.string = c("", "NA"))
dim(training)
dim(testing)
head(training)
head(testing) 

#Eliminate columns with 80% of NAs from training and test data. 
training1 <- training[, colSums(is.na(training)) < nrow(training)*0.8]
dim(training1)
names(training)

testing1 <- testing[, colSums(is.na(testing)) < nrow(testing)*0.8]
dim(testing1)
names(training)


#split the training data into a training set and a cross-validation set
inTrain <- createDataPartition(y = training1$classe, p = 0.7, list = FALSE)
training2 <- training1[inTrain,]
CrossValidation <- training1[-inTrain,]
dim(training2)
dim(CrossValidation)

#Skip the first 5 variables that may cause confusion to the algorithm
training3 <- training2[, 6:60]
names(training3)

CrossValidation1 <- CrossValidation[, 6:60]
names(CrossValidation1)

#Fit the data to a random forest classication model to predict classe using all variables in the given data
fitRF <- train(classe~., data = training3, method = 'rf', ntree = 15)
fitRF$finalModel

#Apply the classification model to the cross-validation sample and examine the accuracy of predictions.
PredCV <- predict(fitRF, CrossValidation1)
confusionMatrix (PredCV, CrossValidation1$classe)

#Applying model to the testing data set.
PredTest <- predict(fitRF, testing1)
PredTest


#Convert predictions to character vector and create function to write predictions to files
predTest1 <- as.character(PredTest)

pml_write_files <- function(x) {
  n <- length(x)
  for (i in 1:n) {
    filename <- paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = F, row.names = F, col.names = F)
  }
}

pml_write_files(predTest1)




