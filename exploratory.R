library(caret)
library(ggplot2)
library(Hmisc)
library(randomForest)
library(e1071)

data <- read.csv("data/pml-training.csv",header = TRUE)[,-c(1:7)]
dim(data)
names(data)
summary(data)
#Remove rows with NAs
data <- data[,colSums(is.na(data))==0]
dim(data)
names(data)
str(data)

#Remove columns with categorical variables with abobe 53 categories and other
#columns filled with NANs in the pml-testing.csv file

data <-data[,-grep("kurtosis|skewness|max|min|avg|stddev|var|amplitude",names(data))]
str(data)
names(data)

inTrain <- createDataPartition(data$classe,p=0.7,list = FALSE)
training <- data[inTrain,]
testing  <- data[-inTrain,]
model <- rfcv(training[,-53],training[,53],cv.fold = 5, step = 0.5,scale="log",recursive = TRUE)
modelFit <- randomForest(training[,-53],training[,53])


predictions1 <- predict(modelFit, newdata=training)
confusionMatrix(predictions1,training$classe)


predictions2 <- predict(modelFit, newdata=testing)
confusionMatrix(predictions2,testing$classe)

evaluation <- read.csv("data//pml-testing.csv",header = TRUE)[-c(1:7)]
evaluation <- evaluation[,colSums(is.na(evaluation))==0]

finalp <- predict(modelFit, newdata=evaluation)
finalp

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(finalp)


#Exploratory Plots
BeltSensors <- training[,grepl("belt|classe",names(training))]
names(BeltSensors)
p <- ggplot(training,aes(roll_belt,pitch_belt,col= classe))
p + geom_point(aes(shape=cut(training$yaw_belt,2)))

p <- ggplot(training,aes(roll_belt,pitch_belt,col= classe))
p + geom_point(aes(shape=cut(training$yaw_belt,3)))