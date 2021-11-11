install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library("readxl")
#  importamos la base de datos
ibd <- read_excel("IBD.xls", sheet = "Data")

############  a) Todas las graficas

# Histograma de la variable "EPS Rating"
hist(ibd$`EPS Rating`, breaks = seq(from=0, to=100, by=10))
hist(ibd$`EPS Rating`, freq=FALSE, breaks = seq(from=0, to=100, by=10))
#lines(density(ibd$`EPS Rating`), lwd=3, col='blue')
hist(ibd$`Relative Price Strength`, breaks = seq(from=0, to=100, by=10))
hist(ibd$`PE Ratio`, breaks = seq(from=0, to=60, by=10))
#cbind( Freq=table(ibd), Cumul=cumsum(table(ibd)), relative=prop.table(table(ibd)))

# Histograma de la variable "EPS Rating" y el color esta dado por "Sales/Margins/ROE"  
ggplot(ibd,aes(x=`EPS Rating`, fill=`Sales/Margins/ROE`))+geom_histogram(bins = 11)
# Grafico"
migrafico<-ggplot(ibd, aes(x=`EPS Rating`, y=`Relative Price Strength`, color=`Sales/Margins/ROE`)) +
  geom_point(size=5) +
#  geom_smooth()+
  labs(title="Diagrama de puntos", x="Calificación EPS", y="Razón precio-beneficio")
migrafico
migrafico+facet_wrap( ~ `Sales/Margins/ROE`, ncol=3)
# Boxplot
p <- ggplot(ibd,aes(`Sales/Margins/ROE`,`PE Ratio`))
p +  geom_boxplot(aes(fill=`Sales/Margins/ROE`))

############  b) calcular el promedio, la media, la mediana, moda, RIQ, CV

#El minimo, máximo, media, mediana, primer y tercer cuartil
  
summary(ibd)
### Tabla de frecuencias
breaks <- seq(from=min(ibd$`EPS Rating`),
              to=max(ibd$`EPS Rating`), length=10)
breaks <- seq(from=min(0),
              to=max(100), length=10)
pop_freq <- cut(ibd$`EPS Rating`, breaks=breaks,
                right=TRUE, include.lowest=TRUE)

table(pop_freq)
#La moda
install.packages("modeest")
library(modeest)
mfv1(ibd$`EPS Rating`)

View(ibd)
# Cuartiles
quantile(ibd$`EPS Rating`, p=c(.25, .5, .75))
# RIQ
IQR(ibd$`EPS Rating`)
# Coeficiente de variación
coeficienteVariacion <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}
coeficienteVariacion(x=ibd$`EPS Rating`, na.rm=T)
