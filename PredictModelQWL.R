
library(corrplot)
library(caret)
library(randomForest)
testing   <- read.csv("pml-testing.csv")
training  <- read.csv("pml-training.csv")

training <- training [c(-1)]

training[training==""] <- NA

##Create a vector to keep track of columns with more NAs
NAcols <- rep(FALSE, ncol(training)) ## default it to no NAs

## iterate through all columns and flag those with lots of NAs to get rid ##of them in the next step
for (i in 1:ncol(training)) {
  if( sum(is.na(training[,i])) > 100) {
    NAcols[i] <- TRUE
  }
}

## eliminate columns with more NAs 
training2 <- training[,!NAcols]

training3 <- training2[,-c(1:6)]

clean1 <- colnames(training3)
clean2 <- colnames(training3[, -53]) # already with classe column removed

testing1 <- testing[clean2]

dim(testing1)


# create dataset for crossvalidation 
inTrain <- createDataPartition(training3$classe, p = 0.7, list=FALSE)
train_subset <- training3[inTrain,]
crossval <- training3[-inTrain,]


model_control <- trainControl(method="cv", number=5, allowParallel=T, verbose=TRUE)
random_forest_model<-train(classe~.,data=train_subset,method="rf",trControl=model_control, verbose = FALSE)

# Model validation on cross validation dataset
pred_sub <- predict(random_forest_model, newdata=crossval)

## Extract the confusion matrix to assess model validity
confMat <- confusionMatrix(pred_sub, crossval$classe)
confMat$table

# Accuracy Calculation 
accuracy <- sum((pred_sub==crossval$classe))/dim(crossval)[1]

# using model for predicting the test dataset 
answer= predict(random_forest_model, testing1)
answer
# function to write the results to files 
qwl_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

qwl_write_files(answer)

