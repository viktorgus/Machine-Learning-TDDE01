# Loading
library("readxl")
library("kknn")
# read file


#Calculates new confussion matrix
confussion = function(pred,real,name){
  pred <- as.numeric(pred>threshold)
  length(pred)
  length(real)
  cat("------------------------------------------\n")
  cat("\n\n STATS for ")
  cat(name)
  conf = table(pred,real)
  cat("\n\n Confussion matrix \n\n ")
  print(conf)
  cat("\n")
  
  mcr = 1-sum(diag(conf))/sum(conf)
  cat(paste("MissRatio; ", mcr))
  cat("\n")
}
data <- read_excel("spambase.xlsx")

#split data 50/50 training and testing
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

threshold=0.5
#Train linear regression model on train-data
logistic <- glm(Spam ~ . ,data=train,family="binomial")

#Get stats for traindata
pred_train <- predict(logistic,train,type="response")
confussion(pred_train, train$Spam, "train")

#get stats for testdata
pred_test <- predict(logistic,test,type="response")
confussion(pred_test,test$Spam,"test")

knnModel = kknn(Spam~.,train,test, k = 30)
knnpred_test = fitted(knnModel)
confussion(knnpred_test,test$Spam, "### knnTest ###")

knnModel = kknn(Spam~.,train,train, k = 30)
knnpred_train = fitted(knnModel)
confussion(knnpred_train,train$Spam, "### knn Train ###")

