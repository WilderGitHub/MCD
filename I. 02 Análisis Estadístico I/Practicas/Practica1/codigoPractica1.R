install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library("readxl")
#  importamos la base de datos
ibd <- read_excel("IBD.xls", sheet = "Data")
# Histograma de la variable "EPS Rating"
hist(ibd$`EPS Rating`, breaks = seq(from=0, to=100, by=10))
hist(ibd$`Relative Price Strength`, breaks = seq(from=0, to=100, by=10))
hist(ibd$`PE Ratio`, breaks = seq(from=0, to=60, by=10))
cbind( Freq=table(ibd), Cumul=cumsum(table(ibd)), relative=prop.table(table(ibd)))

# Histograma de la variable "EPS Rating" y el color esta dado por "Sales/Margins/ROE"  
ggplot(ibd,aes(x=`EPS Rating`, fill=`Sales/Margins/ROE`))+geom_histogram(bins = 11)
# Grafico"
migrafico<-ggplot(ibd, aes(x=`EPS Rating`, y=`PE Ratio`, color=`Sales/Margins/ROE`)) +
  geom_point(size=5) +
#  geom_smooth()+
  labs(title="Diagrama de puntos", x="Calificación EPS", y="Razón precio-beneficio")
migrafico
migrafico+facet_wrap( ~ `Sales/Margins/ROE`, ncol=3)
# Boxplot
p <- ggplot(ibd,aes(`Sales/Margins/ROE`,`PE Ratio`))
p +  geom_boxplot(aes(fill=`Sales/Margins/ROE`))

summary(ibd)
