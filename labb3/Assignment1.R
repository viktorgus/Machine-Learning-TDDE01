set.seed(1234567890)
library(geosphere)
library(ggplot2)

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
data.merge <- merge(stations,temps,by="station_number")
data.merge$date = as.Date(data.merge$date,format='%Y-%m-%d')
data.merge$time = strptime(data.merge$time,format = "%H:%M:%S")

gaussianKernel = function(h,diff){
  u = (diff)/h
  return(exp(-u^2))
}

  h_distance <- 10000000 # These three values are up to the students
  h_date <- 5
  h_time <- 10

  a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
  
date <- "2013-11-04" # The date to predict (up to the students)
date = as.Date(date,format='%Y-%m-%d')

times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00", "12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00", "24:00:00")
times = strptime(times,format = "%H:%M:%S")

##Filter out dates and times after the user input
st = data.merge[which((data.merge$date < date) | (data.merge$date==date && data.merge$time<= time)),]

temp <- vector(mode="double",length=length(times))

#Distance Kernel
station.positions = cbind(st$longitude,st$latitude)
it.dist = distHaversine(station.positions,c(a,b))
it.distKernel = gaussianKernel(h_distance,it.dist)

#Date Kernel
it.dateKernel = gaussianKernel(h_date,as.double(date-st$date))


  for(p in 1:length(times)){
    #Time Kernel
    it.timeKernel = gaussianKernel(h_time,as.double(times[p]-st$time))
    #Add all kernels and calculate weighted sums to predict temp
    it.totKernel =  it.distKernel +  it.timeKernel + it.dateKernel 
    temp[p] = sum(it.totKernel * st$air_temperature)/sum(it.totKernel)
  }

#Result
plot(col="blue",x=times,y=temp, type="o", main=sprintf("Temperature for %s",date),xlab="Hour",ylab="Temperature",sub=sprintf("longitude: %s latitude: %s",a,b))

#test distance kernel
plot(x=it.dist,y=it.distKernel,xlab=sprintf("Distance from %s %s to station",a,b),ylab="distKernel",main="distKernel by distance",col="red")
