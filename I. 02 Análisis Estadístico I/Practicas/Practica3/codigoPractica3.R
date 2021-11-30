#Ejercicio 18 pag 242
sd <- 8.2
mean<-30

# a)
x<-40
probZ<-pnorm(x,mean,sd,lower.tail = FALSE)
#pMayorzZ<-1-probZ
#pMayorzZ
palmenos<-probZ
palmenos
# b)
x<-20
probZ<-pnorm(x,mean,sd,lower.tail = TRUE)
pAlomas<-probZ
pAlomas
# c)  
probabilidad <- 0.9
qnorm(probabilidad,mean,sd,lower.tail = TRUE)

# Ejercicio 24 pag 242
volumenes<-c(214,202,174,163,198,171,265,212,211,194,201,211,180)
#a)
media<-mean(volumenes)
media
ds<-sd(volumenes)
ds
#b)
x<-180
probZ<-pnorm(x,media,ds,lower.tail = FALSE)
probMayor<-probZ
probMayor

# c)

prob<-0.05
qnorm(prob,media,ds,lower.tail = FALSE)

# Distribuciones muestrales
#Ejercicio 22 pag 278
media<-51800
n<-120
desv<-4000
error.est<-desv/sqrt(n)
x1<-51300
x2<-52300
z1<-(x1-media)/error.est
z2<-(x2-media)/error.est
1-(pnorm(z2)-pnorm(z1))

x <- seq(media-desv,media+desv , by = 100)
y <- dnorm(x, mean = media, sd = error.est)
plot(x,y)


