install.packages("readxl")
library("readxl")
install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)
pacman::p_load(pacman,party,rio,tidyverse)
install.packages("modeest")
library(modeest)

exportaciones<-read_excel("exportaciones.xlsx")  %>% as_tibble()
head(exportaciones)
indices<-read_excel("indices.xlsx")  %>% as_tibble()
head(indices)

# ########
# exportacionesMes<-exportaciones%>%
#   filter(Fecha >=as.Date("2021-01-01")&Fecha <as.Date("2021-02-01"))
#   filter(Pais ==("Brasil"))
# exportacionesMes

# Histogramas
ggplot(exportaciones,aes(x=Solicitudes, fill=Tipo, alpha=1))+geom_histogram(bins = 100)

miHistograma = function(x, de, a, pasos ){hist(x, breaks = seq(from=de, to=a, by=pasos))}
miHistograma(exportaciones$Solicitudes,0,400,20)
miHistograma(exportaciones$Aprobadas,0,100,5)
#miHistograma(indices$Tipo["AGRICOLAS"],160,190,5)
#miHistograma(indices$Tipo$MINERALES,130,160,5)
#miHistograma(indices$Tipo$HIDROCARBUROS,90,140,5)
miHistograma(indices$Valor,90,200,5)

###### Barras
barplot(table(exportaciones$`Tipo`),main="Tipo")
par(mar=c(5,15,4,4))
barplot(table(exportaciones$Pais),  horiz=T , las=1, main="Pais Destino", col="#69b3a2")
barplot(table(exportaciones$Departamento),  horiz=T , las=1, main="Departamento", col="light blue")

##### Pie chart

totalesPorTipo <- aggregate(Aprobadas ~ Tipo, data = exportaciones, sum)

nombres=totalesPorTipo$Tipo
nombres
etiquetas = paste(nombres)
#etiquetas = paste(nombres, " (", pct(Aprobadas), "%)", sep = "")
pie(totalesPorTipo$Aprobadas, col = terrain.colors(4),labels = etiquetas)

# Boxplot
miBoxplot = function(bd,color, x){
  p <- ggplot(bd,aes(color,x))
  p +  geom_boxplot(aes(fill=color))   }
boxplot(exportaciones$Solicitudes)
miBoxplot(exportaciones,exportaciones$Tipo,exportaciones$Solicitudes)
boxplot(exportaciones$Aprobadas)
miBoxplot(exportaciones,exportaciones$Tipo,exportaciones$Aprobadas)
############  Estadísticos
medidas = function(x){
  Medida<-c("Media","Mediana","Moda","Cuartil 25", "Cuartil 75","IQR", "Coef. de variación")
  Valor <-c(mean(x), median(x),mfv1(x),quantile(x, p=c(.25, .75)),IQR(x),sd(x)/mean(x))
  tabla = data.frame(Medida,Valor)
  tabla }

medidas(exportaciones$Solicitudes)
medidas(exportaciones$Aprobadas)

######  Asociación

asociacion = function(x,y){
  Medida<-c("Covarianza","Coef. de Correlación")
  Valor<-c(cov(x, y), cor(x,y))
  tabla = data.frame(Medida,Valor)
  tabla}
asociacion(exportaciones$Solicitudes,exportaciones$Aprobadas)

### scatter
ggpairs(exportaciones,
        columns=c("nombre"))

plot(exportaciones$Solicitudes,exportaciones$Aprobadas, xlab = "Trámites", ylab = "Efectivas", main = "Correlaciónttt")
recta = lm(exportaciones$Aprobadas ~ exportaciones$Solicitudes)
abline(recta, col = "darkgreen", lwd = 2)

### scatter colores
ggplot(exportaciones, aes(x=Solicitudes, y=Aprobadas, color=Tipo)) +
  geom_point(size=2,alpha=.3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)#+ylim(0,100)+xlim(0,100)
### scatter colores
ggplot(exportaciones, aes(x=Solicitudes, y=Aprobadas, color=Departamento)) +
  geom_point(size=2,alpha=.3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)#+ylim(0,100)+xlim(0,100)


######      Solicitudes   #########
normales<-function(campo,confianza){
  media<-mean(campo)
  n<-length(campo)
  desv<-sd(campo)
  NConfianza<-c(confianza)
  Z<-qnorm((1-NConfianza)/2,lower.tail = FALSE)
  error.est <- desv/sqrt(n)
  margen.error <- Z * error.est
  lim.inf <- media - margen.error
  lim.sup <- media + margen.error
  cat("limite inferior: ", lim.inf, ", limite superior: ",lim.sup)
}
normales(exportaciones$Solicitudes,0.99)
normales(exportaciones$Aprobadas,0.99)

####### Hipotesis ##########

hipotesis<-function(campo,LaMedia){
  mediamuestral <- media<-mean(campo)
  desvia <- desv<-sd(campo)
  muestra <- n<-length(campo)
  media <- LaMedia
  estadistico <- (mediamuestral - media) / (desvia / sqrt(muestra))
  pnorm(estadistico)
}
summary(exportaciones$Solicitudes)
hipotesis(exportaciones$Solicitudes,130)
summary(exportaciones$Aprobadas)
hipotesis(exportaciones$Aprobadas,22)



####### grafico de lineas Aprobaciones ##########
totalMeses <- aggregate(Aprobadas ~ Fecha+Tipo, data = exportaciones, sum)
ggplot(totalMeses, aes(x=Fecha, y=Aprobadas, color=Tipo))+
geom_line(size=2,alpha=1)

#totalMesesTipo<-totalMeses%>%
#  filter(Tipo=="MINERALES")
#ggplot(totalMesesTipo, aes(x=Fecha, y=Aprobadas, color=Tipo))+
#  geom_line(size=2,alpha=1)

######### grafico de lineas indices
#indicesTio<-indices%>%
#  filter()
#ggplot(indices, aes(x=Fecha, y=IndiceMinerales))+
#  geom_line(size=2,alpha=1)


ggplot(indices, aes(x=Fecha, y=Valor, color=Tipo))+
  geom_line(size=2,alpha=1)

#indiceTipo<-indices%>%
#  filter(Indices=="MINERALES")
#ggplot(indiceTipo, aes(x=Fecha, y=Valor))+
#  geom_line(size=2,alpha=1)

#plot(indiceTipo$Valor,totalMesesTipo$Solicitudes)
total <- merge(totalMeses,indices,by=c("Fecha","Tipo"))
total
ggplot(total, aes(x=Aprobadas, y=Valor, color=Tipo)) +
  geom_point(size=2,alpha=.3) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)#+ylim(50,200)#+xlim(0,100)

