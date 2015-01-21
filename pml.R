library(caret)

# Load 
training <- read.csv("E:/Git/PracticalMachineLearning/pml-training.csv", header = TRUE)
test <- read.csv('E:/Git/PracticalMachineLearning/pml-testing.csv')

training$classe <- as.factor(training$classe)

training <- training[, -c(1:7)]
test <- test[, -c(1:7)]

naCount <- apply(training, 2, function(x) {sum(is.na(x))})
nz <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, which(naCount == 0 & nz$nzv == FALSE)]

# Split
ti <- createDataPartition(y = training$classe, p=0.6, list=FALSE)
trainPartition <- training[ti, ]
testPartition <- training[-ti, ]

# Train
set.seed(1234)
model <- train(classe ~ ., method = "rf", data = trainPartition)
accuracy <- predict(model , testPartition)
print(confusionMatrix(accuracy, testPartition$classe))

# Cross Validation
set.seed(1234)
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model_10_CV <- train(classe ~ ., method="rf",  data=trainPartition, trControl = tc)
accuracy_10_CV <- predict(model_10_CV , testPartition)
print(confusionMatrix(accuracy_10_CV, testPartition$classe))

# Predict 
pred <- predict(model_10_CV, test)
print(pred)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("E:/Git/PracticalMachineLearning/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred)