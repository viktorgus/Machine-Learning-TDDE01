# Loading
library("readxl")
library("kknn")
library("ggplot2")
# read file
data <- read_excel("machines.xlsx")

#loglikelihood for a vector of observations and its theta value
loglike = function(vector,theta){
  n = length(vector)
  return (n*log(theta) - theta*sum(vector))
}

#loglike with prior distrubution for theta
bayesian = function(const,theta,vector){
  prior = (log(const)-const*theta)
  return (prior + loglike(vector,theta))
}

# inverse of CDF for exponential distrubution
getLength = function(prob,theta){
  return( -log(1-prob)/theta )
}

thetaX = seq(0,4,0.01)
#sample 6 first points
data_6 = data$Length[1:6]

#find theta with highest corresponding likelihood for loglike and loglike with prior
theta1 = optimize(loglike,c(0,100),vector=data$Length,maximum="true")$maximum
theta2 = optimize(bayesian,c(0,100),const=10,vector=data$Length,maximum="true")$maximum

#p: loglike on all observations p2: loglike on 6 first observations p3: loglike w/ prior on all observation
p <- qplot(thetaX,loglike(data$Length,thetaX),col="all data",xlab = "Theta", ylab ="log-probability")
p2 <- geom_point(aes(thetaX,loglike(data_6,thetaX),col="6-point sample"))
p3 <- geom_point(aes(thetaX,bayesian(10,thetaX,data$Length),col="prob w/prior"))
p+p3

set.seed(12345)

#histogram over observed vals
h1 <- hist(data$Length,xlim=c(0,4),ylim=c(0,25),main="Observed",xlab="Length",ylab="Ammount", col="red")
#generate new vals with optimized theta
newVals = getLength(runif(50, 0, 1),theta1)
#histogram over new vals
h2 <- hist(newVals,xlim=c(0,4),ylim=c(0,25),main="Generated",xlab="Length",ylab="Ammount",col="blue")
h1
h2

