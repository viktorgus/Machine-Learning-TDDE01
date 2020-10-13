library("readxl")
library("ggplot2")
library("e1071")
library("MASS")


library(tree)


data = read_xls("creditscoring.xls")

#Ex. 1
#Split data into test, validation and train
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 

mcr = function(conf){
  return (1 - sum(diag(conf))/sum(conf))
}
#Ex 2.


tree_dev = tree(as.factor(good_bad) ~ ., data = train, split = "deviance")
tree_gini = tree(as.factor(good_bad) ~ ., data = train, split = "gini")

set.seed(12345)
train_dev_pred = predict(tree_dev, newdata = train, type="class")
train_gini_pred = predict(tree_gini, newdata = train, type="class")

test_dev_pred = predict(tree_dev, newdata = test, type="class")
test_gini_pred = predict(tree_gini, newdata = test, type="class")

conf_test_dev = table(test_dev_pred,test$good_bad)
conf_test_gini = table(test_gini_pred,test$good_bad)
conf_train_dev = table(train_dev_pred, train$good_bad)
conf_train_gini = table(train_gini_pred, train$good_bad)

mcr(conf_train_dev)
mcr(conf_train_gini)
mcr(conf_test_dev)
mcr(conf_test_gini)


##Excercise 2
leaves = 10

trainScore=rep(0,leaves)
testScore=rep(0,leaves)


for(i in 2:leaves) {
  prunedTree = prune.tree(tree_dev, best = i)
  pred = predict(prunedTree, newdata=valid, type="tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
}

qplot(x=2:leaves,y= trainScore[2:leaves], ylim=c(270,570), ylab="train/test scores", xlab="leaves") + geom_point(aes(x=2:leaves,y= testScore[2:leaves]), col="red")+labs(title="Decviance score",subtitle="for train(black) and test(red)")
optTree = prune.tree(tree_dev, best = 4)
summary(optTree)

plot(optTree)
text(optTree)
title("Optimal pruned tree")

testpredict = predict(optTree, newdata = test, type = "class")
testConf = table(testpredict, test$good_bad)
optTree_testMCR = 1-sum(diag(testConf)/sum(testConf))

# Excercise 4
naive = naiveBayes(as.factor(good_bad)~., data=train)
test_pred = predict(naive, newdata = test)
test_pred_conf = table(test_pred,test$good_bad)
naiveMCR_test = 1- sum(diag(test_pred_conf))/sum(test_pred_conf)
naiveMCR_test

train_pred = predict(naive, newdata = train)
train_pred_conf = table(train_pred,train$good_bad)
naiveMCR_train = 1- sum(diag(train_pred_conf))/sum(train_pred_conf)

#Excercise 5
phi = seq(0.05,0.95,0.05)
TPR.naive = rep(0,length(phi))
FPR.naive = rep(0,length(phi))
TPR.tree = rep(0,length(phi))
FPR.tree = rep(0,length(phi))

pred_naive = predict(naive,newdata=test, type="raw")
pred_tree = predict(optTree,newdata=test)

for(i in 1:length(phi)){
  predict.naive = ifelse(pred_naive[,2]>phi[i],"good","bad")
  predict.tree = ifelse(pred_tree[,2]>phi[i],"good","bad")
  
  tN = table(predict.naive,test$good_bad)
  TPR.naive[i] = tN[2,2]/(tN[2,2]+tN[2,1])
  FPR.naive[i] = tN[1,2]/(tN[1,2]+tN[1,1])
  
  tT = table(predict.tree,test$good_bad)
  TPR.tree[i] = tT[1,2]/(tT[1,2]+tT[1,1])
  FPR.tree[i] = tT[1,1]/(tT[1,2]+tT[1,1])
}
qplot(x=FPR.naive,y=TPR.naive)
qplot(x=FPR.tree,y=TPR.tree) 

# excercise 6
naive = naiveBayes(as.factor(good_bad)~., data=train)
naive_pred = predict(naive, newdata = test,type="raw")
naive_pred = ifelse(naive_pred[,1]/naive_pred[,2]>10,"good","bad")

test_pred_conf_wLoss = table(naive_pred,test$good_bad)
naiveMCR_test_wLoss = 1- sum(diag(test_pred_conf_wLoss))/sum(test_pred_conf_wLoss)

