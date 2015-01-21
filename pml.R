library(caret)

#load data from disk
train <- read.csv("E:/Git/PracticalMachineLearning/pml-training.csv", header = TRUE)
test <- read.csv('E:/Git/PracticalMachineLearning/pml-testing.csv')
train$classe <- as.factor(train$classe)

#split the training set in 60% for training and 40% for test
ti <- createDataPartition(y = train$classe, p=0.6, list=FALSE)
trainPart <- train[ti, ]
testPart <- train[-ti, ]

#random seed
set.seed(1234)

model <- train(classe ~ .,data=trainPart,  method="lda")

# testing accuracy
accuracy <- predict(model , testPart)
print(confusionMatrix(accuracy, testPart$classe))

# 10-fold Cross Validation
controlf <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model_CV <- train(classe ~ ., method="lda",  data=trainPart, trControl=controlf)

# Prediction on the test set
pred <- predict(model_CV, test)
print(pred)

for(i in 1:length(res)) {
  filename = paste("E:/Git/PracticalMachineLearning/res_", i, ".txt", sep="")
  write.table(as.vector(pred[i]), file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
}
