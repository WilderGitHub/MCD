############## Ejercicio 1 ########
###### Cluster  #########
install.packages("readxl")
install.packages("useful")
library (readxl)
library(useful)
library(GGally)

exportaciones<-read_excel("exportaciones.xlsx")
head(exportaciones)
summary(exportaciones)
exportaciones$pais<-as.factor(exportaciones$pais)

ggpairs(exportaciones[,2:5], lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

exportaciones[,2:5]<-scale(exportaciones[,2:5])
head(exportaciones)
##
distancias<-dist(exportaciones[,2:5])
distancias
##agrupamiento
agrupamiento<-hclust(distancias)


## k grupos
(grupos<-cutree(agrupamiento, k=3))
###
plot(agrupamiento,hang=-1,cex=0.7, labels=exportaciones$pais, main="Cluster
de exportaciones por País destino")

rect.hclust(agrupamiento, k=3, border="red")

# grafico kmeans
train <- exportaciones[, which(names(exportaciones) != "pais")]
set.seed(123)
w <- kmeans(x=train, centers=3)
w

plot(w, data=train)



############## Ejercicio 2 ########
###### Discriminante  #########

p<-read.delim("portafolio.txt", header = TRUE, sep = "\t")
p
p$portafolio <- as.factor(p$portafolio)


# descripción de los datos

library(ggplot2)
install.packages('ggpubr')
library(ggpubr)

po <- ggplot(data = p, aes(x = calificacion, color = portafolio,fill=portafolio)) +
  geom_histogram(position = "identity",alpha = 0.5) 
po +scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
v <- ggplot(data = p, aes(x = volumen, color = portafolio, fill=portafolio)) +
  geom_histogram(position = "identity",alpha = 0.5)
v+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
pr <- ggplot(data = p, aes(x = precio, color = portafolio,
                                             fill=portafolio)) +
  geom_histogram(position = "identity",alpha = 0.5)
pr+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Correlaciones

library(GGally)
ggpairs(p, lower = list(continuous = "smooth", ggplot2::aes(colour=portafolio)),
        diag = list(continuous = "bar"), axisLabels = "none")

data(p)
ggpairs(p, columns = 2:4, ggplot2::aes(colour=portafolio, alpha=0.5)) 

# LDA 
install.packages('MASS')
library(MASS)
modeloDiscriminante <- lda(formula = portafolio ~ calificacion +volumen + precio,
                           data = p)

modeloDiscriminante

# predicción

nuevoInput <- data.frame(calificacion = 190, volumen = 140,
                         precio = 48)
predict(object = modeloDiscriminante, newdata = nuevoInput)


pairs(x = p[, -4], col = c("firebrick", "green3", "blue")[p$portafolio],
      pch = 20)


install.packages("klaR")
library(klaR)
partimat(portafolio ~ calificacion + volumen + precio,
         data = p, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1",  "skyblue2"),
         col.mean = "firebrick")




