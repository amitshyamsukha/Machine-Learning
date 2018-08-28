

library(kernlab)
library(readr)
library(caret)

setwd("C:/Amit/Upgrad/SVM_dataset/SVM Dataset1")

## Read the training data set 
mnist_dataset <- read.csv("mnist_train.csv" , header = F)

###Structure of the dataset
str(mnist_dataset)

## Examine few records 
head(mnist_dataset)

#Exploring the data
summary(mnist_dataset)

## Check if there is any missing value in data set 
column_null_check <- sapply (mnist_dataset , function(x) sum(is.na(x) ))   

which(column_null_check >0 ) ## No null value 

column_min_value <- sapply (mnist_dataset , function(x) min(x) )
which(column_min_value <  0)  ##  None 
which(column_min_value >  0)  ## None 
length(which(column_min_value == 0))   ## 785. All min value is 0 

column_max_value <- sapply (mnist_dataset , function(x) max(x) )
which(column_max_value >  255)  ## None 

## So all the values are within range of 0 and 255. This is within permissable limit. So no outlier is there

## Check if there is any duplicate data in mnist_dataset data set 
which(duplicated(mnist_dataset)) ## 0 

## Plot couple of digits to check how images are looking 

flip <- function(matrix){
  apply(matrix, 2, rev)
}

par(mfrow=c(3,3))
for (i in 20:28){
  digit <- flip(matrix(rev(as.numeric(mnist_dataset[i,-c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}

par(mfrow=c(3,3))
for (i in 5000:5008){
  digit <- flip(matrix(rev(as.numeric(mnist_dataset[i,-c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


par(mfrow=c(3,3))
for (i in 10000:10008){
  digit <- flip(matrix(rev(as.numeric(mnist_dataset[i,-c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}




# Changing output variable "V1" to factor type 
mnist_dataset$V1<-factor(mnist_dataset$V1)

## Since data set is huge. SO taking  10% sample of  mnist_dataset dataset to train the model 

train.indices = sample(1:nrow(mnist_dataset), 0.1*nrow(mnist_dataset))
train = mnist_dataset[train.indices, ]

## Make sure we have enough representation of each digits in the sample taken
table ( train$V1)  


## Load the test data set 
mnist_test_dataset <- read.csv("mnist_test.csv" , header = F)
## Take 50% of test data set for model test 
test.indices = sample(1:nrow(mnist_test_dataset), 0.5*nrow(mnist_test_dataset))
test = mnist_test_dataset[test.indices, ]
test$V1<-factor(test$V1)

#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")

## Predict the output 
Eval_linear<- predict(Model_linear, test)

confusionMatrix(Eval_linear,test$V1)   

## Overall Accuracy : 0.915. But sensitivity is too low for few digit e.g. 3, 5 and 8 ( close to 80%). 
### So we need to move from linear to RBF


#Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)   

### Overall  Accuracy increased to  .95 . Accuracy for all digits increased  to more than 90% now. 
## So RBF kernel is better than linear one. 

#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(8)
grid <- expand.grid(.sigma=c(0.50e-07, 1.50e-07, 2.50e-07), .C=c(1,2) )

# Performing 5-fold cross validation
fit.svm_radial <- train(V1~., data=train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 2.5e-07 and C = 2 for maximum accuracy 
## Since sigma is very low, data is not very linear 

# Plotting model results
plot(fit.svm_radial)

  
######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, test)
confusionMatrix(evaluate_non_linear, test$V1)

# Accuracy    - 97.4%
# Sensitivity - Around 95% for all digits 
# Specificity - Around 99%% for all digits