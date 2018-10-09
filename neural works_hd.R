HD <- na.omit(heart_disease_clean)
HD['hdpresence']= ifelse(HD['num']>0,1,0)
str(HD)

a= vector()
minmax <- function(a){
  b=(a- min(a))/(max(a)-min(a))
  return(b)
}

HD[,1:13]<- minmax(HD[,1:13])


library(nnet) # Requires package nnet
net.dat <- nnet(hdpresence ~.-num, data =HD, size = 3)
table(round(net.dat$fitted.values, 1)) # If fitted values are all the same, rerun nnet
net.dat$wts # Weights
hist(net.dat$wts)
summary(net.dat)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(net.dat,wts.only= T)
plot.nnet(net.dat)
