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
miHistograma(indices$`IndiceAgricolas`,160,190,5)
miHistograma(indices$`IndiceMinerales`,130,160,5)
miHistograma(indices$`IndiceCombustibles`,90,140,5)
miHistograma(indices$`IndiceTotal`,110,150,5)

###### Barras
barplot(table(exportaciones$`Tipo`),main="Tipo")
barplot(table(exportaciones$`Pais`),main="Pais Destino")
barplot(table(exportaciones$`Departamento`),main="Departamento exportador")
##### Pie chart

totalesPorTipo <- aggregate(Aprobadas ~ Tipo, data = exportaciones, sum)
nombres=totalesPorTipo$Tipo
etiquetas = paste(nombres, " (", pct, "%)", sep = "")
pie(totalesPorTipo$Aprobadas, col = terrain.colors(4),labels = etiquetas)

# Boxplot
miBoxplot = function(bd,color, x){
  p <- ggplot(bd,aes(color,x))
  p +  geom_boxplot(aes(fill=color))   }
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

plot(exportaciones$Solicitudes,exportaciones$Aprobadas, xlab = "sfsadf", ylab = "wyyyyyyy", main = "sttttt")
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

