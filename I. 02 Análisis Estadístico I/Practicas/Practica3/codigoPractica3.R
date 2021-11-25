sd <- 5000
mean<-36500
x<-40000
probZ<-pnorm(x,mean,sd,lower.tail = TRUE)
pMayorzZ<-1-probZ
pMayorzZ
