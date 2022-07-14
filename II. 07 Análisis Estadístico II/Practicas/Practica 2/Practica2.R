####  Ejercicio 1.a.i

#install.packages("BSDA")
library(BSDA)
ventas<-c(0.1,0.4, 0.7, 0.5, 0.8, 0.35, 0.68, 0.91, 0.93,0.75)
SIGN.test(ventas,md=0.8, alternative="two.sided", conf.level = 0.95)
SIGN.test(ventas,md=0.8,alternative = "greater",conf.level = 0.95)
SIGN.test(ventas,md=0.8,alternative = "less",conf.level = 0.95)

####  Ejercicio 1.a.ii
library(MASS)
ventas<-c(0.1,0.4, 0.7, 0.5, 0.8, 0.35, 0.68, 0.91, 0.93,0.75)

wilcox.test(ventas,mu=0.8,exact=T,alternative="t", conf.int=0.95)

####  Ejercicio 1.a.iii

install.packages("exactRankTests")
library(exactRankTests)
ventas<-c(0.1,0.4, 0.7, 0.5, 0.8, 0.35, 0.68, 0.91, 0.93,0.75)
ventasantes<-c(0.2,0.3, 0.6, 0.7, 0.7, 0.4, 0.6, 0.8, 0.65,0.55)
wilcox.exact(ventas,ventasantes,exact = T, alternative = "t",conf.int = 0.95)
wilcox.exact(ventas,ventasantes,exact = F, alternative = "t",conf.int = 0.95)


####  Ejercicio 1.a.iv

comerciales<-c(77,79,89,79,81,95)
hipotecarios<-c(77,78,78,79,80,90)
cooperativas<-c(96,98,89,87,90,92)

kruskal.test(list(comerciales,hipotecarios,cooperativas))


####  Ejercicio 1.b.i

library(BSDA)
ofimaticaT1<-c(80,81,82,90,91,87,79,81,89)
ofimaticaT2<-c(90,83,79,92,93,88,77,78,88)
SIGN.test(ofimaticaT2,md=85, alternative="two.sided", conf.level = 0.95)
SIGN.test(ofimaticaT2,md=85,alternative = "greater",conf.level = 0.95)
SIGN.test(ofimaticaT2,md=85,alternative = "less",conf.level = 0.95)


####  Ejercicio 1.b.ii
library(MASS)
ofimaticaT2<-c(90,83,79,92,93,88,77,78,88)

wilcox.test(ofimaticaT2,mu=0.8,exact=T,alternative="t", conf.int=0.95)

####  Ejercicio 1.b.iii


library(exactRankTests)
ofimaticaT1<-c(80,81,82,90,91,87,79,81,89)
ofimaticaT2<-c(90,83,79,92,93,88,77,78,88)

wilcox.exact(ofimaticaT1,ofimaticaT2,exact = T, alternative = "t",conf.int = 0.95)
wilcox.exact(ofimaticaT1,ofimaticaT2,exact = F, alternative = "t",conf.int = 0.95)

####  Ejercicio 1.b.iv

euros<-c(1,1,1.4,1.35,2,1.7,0.4,0.9)
eurodolares<-c(0,25,1.0,1.01,1.9,1.6,0.8,0.84)
agencias<-c(1,14,1.38,1.32,2.1,1.8,1.4,1.9)
bonos<-c(0.4,0.3,0.35,0.2,0.7,0.45,0.8)

kruskal.test(list(euros,eurodolares,agencias,agencias))


####  Ejercicio 2 ###################1

install.packages("readxl")
install.packages("ggfortify")
install.packages("aTSA")
install.packages("forecast")
install.packages("ts")
install.packages("seasonal")
install.packages("fpp2")
install.packages("tseries")

library (readxl)
library(tidyverse)
library(ggfortify)
library(aTSA)
library(forecast)
library(ggplot2)
library(ts)
library(tidyverse)
library(seasonal) 
library(fpp2)
library(tseries)

#leemos la información de exportación de castaña
castania<-read_excel("castania.xlsx")
head(castania)
sum(is.na(castania))

frequency(castania)
summary(castania)
castania
str(castania)
castania$fecha <- as.Date(castania$fecha, "%Y-%m-%d")
str(castania)
castania2 <- ts(castania$castana, frequency = 12, start = 2003)
castania2


cycle(castania2)
boxplot(castania2~cycle(castania2))
summary(castania2)

#graifco

dt<-aggregate(castania2,FUN=mean)
dt
df<-fortify(castania2)
df
df2<-fortify(dt)
df2
df2$Index<-as.Date(as.character(df2$Index),format = "%Y")

colors<-c("Exportaciones"="#56B4E9","Promedio"="#E69F00")

ggplot(mapping = aes(x=Index,y=Data))+
  geom_line(data=fortify(df,melt=TRUE), aes(color="Exportaciones"))+
  geom_line(data=fortify(df2,melt=TRUE),aes(color="Promedio"))+
  labs(title="Exportaciones de Castaña",
       x="Meses)",
       y="Millones de $us",
       color="Legend")+
  scale_color_manual(values = colors)

#graifco

boxplot(castania2~cycle(castania2),
        xlab="Mes",ylab="Millones de $us",main="Exportaciones de Castaña",
        col="#0072B2",
        names=c("Ene","Feb","Mar","Abr","May","Jun",
                "Jul","Ago","Sep","Oct","Nov","Dic"))

# grafico
ggplot(castania, aes(castania$castana)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()# + xlim(c(0, 1000))

#descomposición
descomposicion <-decompose(castania2,"multiplicative")
autoplot(descomposicion)
# test 
adf.test(castania2)

#
arima<-auto.arima(castania2)
arima

ggtsdiag(arima)

# predicción
prediccion<-forecast(arima,level = c(95),h=36)
autoplot(prediccion)
prediccion
