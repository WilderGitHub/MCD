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



# Distribuciones muestrales
#Ejercicio 22 pag 278
media<-51800
n1<-60
n2<-120
desv<-4000
error.est1<-desv/sqrt(n1)
error.est2<-desv/sqrt(n2)
x1<-51300
x2<-52300
za<-(x1-media)/error.est1
zb<-(x2-media)/error.est1
1-(pnorm(zb)-pnorm(za))
# grafico
x <- seq(media-3*error.est1,media+3*error.est1 , by = 100)
y1 <- dnorm(x, mean = media, sd = error.est1)
y2 <- dnorm(x, mean = media, sd = error.est2)
plot(x,y2)
lines(x,y1)
lines(x,y2, col="red",lty=2)

# Ejercicio 26 pag 279

media<-939
n<-c(30,50,100,400)
desv<-245
error.est<-c(desv/sqrt(n))
q<-25
x1<-media -q
x2<-media +q
z1<-(x1-media)/error.est
z2<-(x2-media)/error.est
1-(pnorm(z2)-pnorm(z1))
# grafico

x <- seq(media-3*error.est[1],media+3*error.est[1] , by = 1)
y <- dnorm(x, mean = media, sd = error.est)
y1 <- dnorm(x, mean = media, sd = error.est[1])
y2 <- dnorm(x, mean = media, sd = error.est[2])
y3 <- dnorm(x, mean = media, sd = error.est[3])
y4 <- dnorm(x, mean = media, sd = error.est[4])
plot(x,y)
lines(x,y1, col="black",pch="o",lty=1)
lines(x,y2, col="red",pch="*",lty=2)
lines(x,y3, col="blue",pch="+",lty=3)
lines(x,y4, col="green",pch="-",lty=4)
legend(1,10,legend=c("y1","y2","y3","y4"), col=c("blue","red","black", "green"),
lty=c(1,2,3,4), ncol=1)

# proporciones
######\ Ejercicio 38 pag 285
p<-0.56
n<-c(400)
#desv<-245
error.est<-c(sqrt((p*(1-p)/n)))
error.est
w<-0.04
x1<-p - w
x2<-p + w
z1<-(x1-p)/error.est
z2<-(x2-p)/error.est
pnorm(z2)-pnorm(z1)
# grafico
x <- seq(p-3*error.est[1],p+3*error.est[1] , by = .01)
y <- dnorm(x, mean = p, sd = error.est)
y1 <- dnorm(x, mean = p, sd = error.est[1])
y2 <- dnorm(x, mean = p, sd = error.est[2])

plot(x,y)
lines(x,y1, col="black",pch="o",lty=1)
lines(x,y2, col="red",pch="*",lty=2)

######\ Ejercicio 40 pag 285
p<-0.76
n<-c(400)
#desv<-245
error.est<-c(sqrt((p*(1-p)/n)))
error.est
w<-0.03
x1<-p - w
x2<-p + w
z1<-(x1-p)/error.est
z2<-(x2-p)/error.est
pnorm(z2)-pnorm(z1)
# grafico

x <- seq(p-3*error.est[1],p+3*error.est[1] , by = .001)
y <- dnorm(x, mean = p, sd = error.est)
y1 <- dnorm(x, mean = p, sd = error.est[1])
y2 <- dnorm(x, mean = p, sd = error.est[2])

#y3 <- dnorm(x, mean = media, sd = error.est[3])
#y4 <- dnorm(x, mean = media, sd = error.est[4])
plot(x,y)
lines(x,y1, col="black",pch="o",lty=1)
lines(x,y2, col="red",pch="*",lty=2)

#### ejercicio 14 pag 314

media<-22.5
n<-54
desv<-4.4
NConfianza<-c(0.90,0.91, 0.92, 0.93,0.94,0.95,0.96,0.97,0.98,0.99)
Z<-qnorm((1-NConfianza)/2,lower.tail = FALSE)
error.est <- desv/sqrt(n)
margen.error <- Z * error.est
lim.inf <- media - margen.error#
lim.sup <- media + margen.error#
Medida<-c("Nivel de Confianza","Limite Inferior","Limite Superior")
tabla = data.frame(NConfianza,lim.inf,lim.sup)
tabla  

#### ejercicio 17 pag 315
install.packages("readxl")
library("readxl")
miami <- read_excel ("miami.xlsx")
media<-mean(miami$calificaciones)
n<-length(miami$calificaciones)
desv<-sd(miami$calificaciones)
NConfianza<-c(0.95)
Z<-qnorm((1-NConfianza)/2,lower.tail = FALSE)
error.est <- desv/sqrt(n)
margen.error <- Z * error.est
lim.inf <- media - margen.error
lim.sup <- media + margen.error
cat("limite inferior: ", lim.inf, ", limite superior: ",lim.sup)


### hipotesis ######

  mediamuestral <- 0.034
  desvia <- 0.02
  muestra <- 40
  media <- 0.041
  estadistico <- (mediamuestral - media) / (desvia / sqrt(muestra))
  estadistico
  2*pnorm(estadistico)
