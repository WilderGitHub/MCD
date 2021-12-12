install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library("readxl")
install.packages("modeest")
library(modeest)
install.packages("GGally")
library(GGally)
###################################################
###  importamos la base de datos asignada IBD.xls ###
####################################################
ibd <- read_excel("IBD.xls", sheet = "Data")

############  Graaficas
# Histogramas
miHistograma = function(x, de, a, pasos ){hist(x, breaks = seq(from=de, to=a, by=pasos))}
miHistograma(ibd$`EPS Rating`,0,100,10)
miHistograma(ibd$`Relative Price Strength`,0,100,10)
miHistograma(ibd$`PE Ratio`,0,60,10)
###### Barras
barplot(table(ibd$`Sales/Margins/ROE`),main="Ventas/margen/ROE")
barplot(table(ibd$`Relative Strength`),main="Fuerza relativa del grupo de industrias")
##### Pie chart
noms = c("A", "B", "C", "D", "E", "F")
pct = round(prop.table(table(ibd$`Sales/Margins/ROE`)) * 100,digits = 2)
etiquetas = paste(noms, " (", pct, "%)", sep = "")
pie(table(ibd$`Sales/Margins/ROE`), col = terrain.colors(7), labels = etiquetas, main = "Sales/Margins/ROE")
# Boxplot
miBoxplot = function(bd,color, x){
  p <- ggplot(bd,aes(color,x))
  p +  geom_boxplot(aes(fill=color))   }
boxplot(ibd$`PE Ratio`)
miBoxplot(ibd,ibd$`Sales/Margins/ROE`,ibd$`PE Ratio`)
boxplot(ibd$`EPS Rating`)
miBoxplot(ibd,ibd$`Sales/Margins/ROE`,ibd$`EPS Rating`)
boxplot(ibd$`Relative Price Strength`)
miBoxplot(ibd,ibd$`Sales/Margins/ROE`,ibd$`Relative Price Strength`)


############  b) calcular el promedio, la media, la mediana, moda, RIQ, CV
medidas = function(x){
  Medida<-c("Media","Mediana","Moda","Cuartil 25", "Cuartil 75","IQR", "Coef. de variación")
  Valor <-c(mean(x), median(x),mfv1(x),quantile(x, p=c(.25, .75)),IQR(x),sd(x)/mean(x))
  tabla = data.frame(Medida,Valor)
  tabla }

medidas(ibd$`PE Ratio`)
medidas(ibd$`EPS Rating`)
medidas(ibd$`Relative Price Strength`)

######  c) la covarianza y el coeficiente de correlación 

asociacion = function(x,y){
  Medida<-c("Covarianza","Coef. de Correlación")
  Valor<-c(cov(x, y), cor(x,y))
  tabla = data.frame(Medida,Valor)
  tabla}
asociacion(ibd$`EPS Rating`,ibd$`PE Ratio`)
asociacion(ibd$`Relative Price Strength`,ibd$`EPS Rating`)
asociacion(ibd$`Relative Price Strength`,ibd$`PE Ratio`)
#######
ggpairs(ibd,
        columns=c("EPS Rating","Relative Price Strength","PE Ratio"))



###################################################
###  importamos la base de datos asignada Broker.xls ###
####################################################
broker <- read_excel("Broker.xls", sheet = "Data")
broker
############  Graaficas
# Histogramas
miHistograma = function(x, de, a, pasos ){hist(x, breaks = seq(from=de, to=a, by=pasos))}
miHistograma(broker$`100 Shares at $50/Share`,0,60,10)
miHistograma(broker$`Online 500 Shares at $50/Share`,0,70,10)

# Boxplot
miBoxplot = function(bd,color, x){
  p <- ggplot(bd,aes(color,x))
  p +  geom_boxplot(aes(fill=color))   }

boxplot(broker$`100 Shares at $50/Share`, main="100 Shares at $50/Share")
boxplot(broker$`Online 500 Shares at $50/Share`,main="Online 500 Shares at $50/Share")


############  b) calcular el promedio, la media, la mediana, moda, RIQ, CV
medidas = function(x){
  Medida<-c("Media","Mediana","Moda","Cuartil 25", "Cuartil 75","IQR", "Coef. de variación")
  Valor <-c(mean(x), median(x),mfv1(x),quantile(x, p=c(.25, .75)),IQR(x),sd(x)/mean(x))
  tabla = data.frame(Medida,Valor)
  tabla }

medidas(broker$`100 Shares at $50/Share`)
medidas(broker$`Online 500 Shares at $50/Share`)


######  c) la covarianza y el coeficiente de correlación 

asociacion = function(x,y){
  Medida<-c("Covarianza","Coef. de Correlación")
  Valor<-c(cov(x, y), cor(x,y))
  tabla = data.frame(Medida,Valor)
  tabla}
asociacion(broker$`100 Shares at $50/Share`,broker$`Online 500 Shares at $50/Share`)

ggpairs(broker,
        columns=c("100 Shares at $50/Share","Online 500 Shares at $50/Share"))

plot(broker$`100 Shares at $50/Share`, broker$`Online 500 Shares at $50/Share`, xlab = "100 Shares at $50/Share", ylab = "Online 500 Shares at $50/Share", main = "Comisiones cobradas por corredores de Bolsa")
recta = lm(broker$`Online 500 Shares at $50/Share` ~ broker$`100 Shares at $50/Share`)
abline(recta, col = "darkgreen", lwd = 2)


