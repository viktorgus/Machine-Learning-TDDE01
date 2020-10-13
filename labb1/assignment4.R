  # Loading
  library("readxl")
  library("ggplot2")
  library("MASS")
  library("glmnet")
  
  
  data <- read_excel("tecator.xlsx")
  
  
  makePolynomialModels = function(maxPol,train,test){
    trainData = data.frame("Moisture" = train$Moisture)
    testData = data.frame("Moisture" = test$Moisture)
    mse_train = c()
    mse_test = c()
    for(i in 1:maxPol){
      trainData[sprintf("Protein %d",i)]=train$Protein^i
      testData[sprintf("Protein %d",i)]=test$Protein^i
      model <- glm(Moisture ~ . ,data=trainData,family="gaussian")
      pred_train <- predict(model,trainData,type="response")
      pred_test <- predict(model,testData,type="response")
      mse_train[i] = sum((trainData$Moisture-pred_train)^2)/nrow(trainData)
      mse_test[i] = sum((testData$Moisture-pred_test)^2)/nrow(testData)
      #cat(sprintf("\n MSE %d Test: %f Train: %f",i,mse_train[i],mse_test[i]))
    }
    p <- qplot(x=seq(1,maxPol,1),y=mse_train,color="Train",ylab="MSE",xlab="polynomial",geom="line")
    p2 <- geom_line(aes(seq(1,maxPol,1),mse_test,color="Test"))
    return(p + p2 )
  
  }
  
  
  
  n=dim(data)[1]
  set.seed(12345)
  id=sample(1:n, floor(n*0.5))
  train=data[id,]
  test=data[-id,]
  
  
  makePolynomialModels(6,train,test)
  
  model <- glm(Fat~ . -Sample -Protein - Moisture,data=data,family="gaussian")
  modelAIC = stepAIC(model,trace=FALSE,direction="backward")        
  length(modelAIC$coefficients)
  
  X = as.matrix(scale(data[,2:101]))
  Y = scale(data$Fat)
  
  model = cv.glmnet(x=X,y=Y,alpha=0,family="gaussian",lambda= seq(0,1,0.001),nfolds=10)
  plot(model)
  
  model$lambda.min
