library("MASS")
library("ggplot2")

data = read.table("australian-crabs.csv",header=TRUE,sep=",")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

mcr = function (table){
  return (1-sum(diag(table))/sum(table))
}

#1.
ggplot(data,aes(x=data$CL,y=data$RW,col=data$sex)) + geom_point() + labs("Sex by RW&CL",x="CL",y="RW")


#2. 
lda2 = lda(sex ~ RW + CL, data = data )
pred2 = predict(lda2,data)$class
table2 = table(pred2,data$sex)
mcr2 = mcr(table2)

miss = data$sex==pred2
ggplot(data,aes(x=data$CL,y=data$RW,col=pred2)) + geom_point() + labs("LDA prediction",x="CL",y="RW")


#3
lda_prior = lda(sex ~ RW + CL, data = data, prior=c(0.1,0.9))
pred_prior = predict(lda_prior,data)$class
table_prior = table(pred_prior,data$sex)
mcr_prior = mcr(table_prior)
miss = data$sex==pred_prior

ggplot(data,aes(x=data$CL,y=data$RW,col=pred_prior)) + geom_point() + labs(title="LDA w Prior",x="CL",y="RW")

#4
Y = as.numeric(data$sex)-1
threshold = 0.5
logistic = glm(Y ~ RW + CL, data = data)
pred_glm = predict(logistic,data)
prediction = pred_glm
prediction[which(prediction>threshold)] = "Male"
prediction[which(prediction<=threshold)] = "Female"

pred_glm>threshold

glmplot = ggplot(data,aes(x=data$CL,y=data$RW,col=prediction)) + geom_point() + labs(x="CL",y="RW",title="Logistic prediction")
table_glm = table(as.numeric(pred_glm>threshold),data$sex)
mcr(table_glm)

w = logistic$coefficients
decision = function(x)  (threshold-w[3]*x - w[1])/w[2]
glmplot + stat_function(fun=decision)


summary(lda2)
