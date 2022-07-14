############## Ejercicio 1 ########
###### Cluster  #########

data(iris)
datos<-iris
View(datos)
# 
datos[,1:4]<-scale(datos[,1:4])
View(datos)
##
distancias<-dist(datos[,1:4])
distancias
##agrupamiento
agrupamiento<-hclust(distancias)

## k grupos
(grupos<-cutree(agrupamiento, k=3))
###
plot(agrupamiento,hang=-1,cex=0.7, labels=datos[,5], main="Cluster
sobre tipos de flor")

##
rect.hclust(agrupamiento, k=3, border="red")


############## Ejercicio 2 ########
###### Discriminante  #########


data("iris")
install.packages("reshape2")
install.packages("knitr")
install.packages("dplyr")
library(reshape2)
library(knitr) 
library(dplyr)
datos_tidy <- melt(iris, value.name = "valor")

kable(head(iris, n = 3))

#####p24

library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

plot1 <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot2 <- ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot3 <- ggplot(data = iris, aes(x = Petal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot4 <- ggplot(data = iris, aes(x = Petal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2
ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE, legend = "bottom")

pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$Species],
      pch = 20)



####p26  Normalidad univariante, normalidad multivariante y homogeneidad de varianza

#representación mediante histograma de cada variable para cada especie 
par(mfcol = c(3, 4))
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}


#representación de cuantiles normales de cada variable para cada especie 
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    qqline(x)
  }
}


#Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
install.packages("reshape2")
install.packages("knitr")
install.packages("dplyr")
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(iris, value.name = "valor")
kable(datos_tidy %>% group_by(Species, variable) 
      %>%summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)))


##La variable petal.width no se distribuye de forma normal en los grupos setosa y versicolor.
##Normalidad multivariante:

install.packages("MVN")
library(MVN)
outliers <- mvn(data = iris[,-5], mvnTest = "hz", multivariateOutlierMethod = "quan")

royston_test <- mvn(data = iris[,-5], mvnTest = "royston", multivariatePlot = "qq")

royston_test$multivariateNormality


hz_test <- mvn(data = iris[,-5], mvnTest = "hz")
hz_test$multivariateNormality

#Ambos test muestran evidencias significativas de falta de normalidad multivariante. 
#El LDA tiene cierta robustez frente a la falta de normalidad multivariante, pero es 
#importante tenerlo en cuenta en la conclusión del análisis.

install.packages("biotools")
library(biotools)
boxM(data = iris[, -5], grouping = iris[, 5])

#El test Box’s M muestra evidencias de que la matriz de covarianza no es constante en 
##todos los grupos, 

##Cálculo de la función discriminantep30

library(MASS)
modelo_lda <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length +
                    Petal.Width, data = iris)
modelo_lda


##Evaluación de los errores de clasificación

predicciones <- predict(object = modelo_lda, newdata = iris[, -5])
table(iris$Species, predicciones$class, dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(iris$Species != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")

###Visualización de las clasificacionesp31
install.packages("klaR")
library(klaR)
partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width,
         data = iris, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")

####Análisis Discriminante Cuadráticop33##### NO FUNCIONA

###Ejemplo QDA con 2 predictores (Se dispone de los siguientes datos simulados.)

set.seed(8558)
grupoA_x <- seq(from = -3, to = 4, length.out = 100)
grupoA_x
grupoA_y <- 6 + 0.15 * grupoA_x - 0.3 * grupoA_x^2 + rnorm(100, sd = 1)
grupoA <- data.frame(variable_z = grupoA_x, variable_w = grupoA_y, grupo = "A")

grupoB_x <- rnorm(n = 100, mean = 0.5, sd = 0.8)
grupoB_y <- rnorm(n = 100, mean = 2, sd = 0.8)
grupoB <- data.frame(variable_z = grupoB_x, variable_w = grupoB_y, grupo = "B")

datos <- rbind(grupoA, grupoB)
datos
str(datos)


plot(datos[, 1:2], col = datos$grupoA, pch = 19)

summary(datos)
summary( datos$grupo) # detalle de la variable
table( datos$grupo ) # niveles o clases de la variable

datos$grupo <- as.factor( datos$grupo ) # transformación de los datos de caracter a factor
str( datos$grupo ) # comprobar que ahora es factor

plot(datos$grupoA,datos$grupoB,
     xlab = "variable_z", # Etiqueta eje x
     ylab = "variable_w", # Etiqueta eje y
     main = "Gráfico de dispersión: Datos simulados", # Titulo principal del grafico
     xlim = c(-3,4), # Rango de los datos de la variable variable_z
     ylim = c(0,8), # Rango de los datos de la variable_w
     col = datos$grupoA, # Pintar por colores la variable uso
     pch = datos$grupoA ) # Distintos simbolos para la variable capacidad

#################################################### #######################

#Exploración gráfica de los datos

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = datos, aes(x = variable_z, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = variable_w, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom")


#########################################################no funcionea
##p35 Normalidad univariante, normalidad multivariante y homogeneidad de varianza

# Representación mediante histograma de cada variable para cada grupo 
par(mfcol = c(2, 2))
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("grupo", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}


#representación de cuantiles normales de cada variable para cada grupo 
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}
##################################

#Contraste de normalidad Shapiro-Wilk para cada variable en cada grupo
library(reshape2)
datos_tidy <- melt(datos, value.name = "valor")
library(dplyr)
datos_tidy %>%
  group_by(grupo, variable) %>% 
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5))


####################
library(MVN)
#outliers <- mvn(data = datos[,-3], mvnTest = "hz", multivariateOutlierMethod = "quan")


#royston_test <- mvn(data = datos[,-3], mvnTest = "royston", multivariatePlot = "qq")


royston_test$multivariateNormality
#hz_test <- mvn(data = datos[,-3], mvnTest = "hz")
hz_test$multivariateNormality

Cálculo de la función discriminante.p38
library(MASS)
modelo_qda <- qda(grupo ~ variable_z + variable_w, data = datos)
modelo_qda


######Evaluación de los errores de clasificación.

predicciones <- predict(object = modelo_qda, newdata = datos)
table(datos$grupo, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(datos$grupo != predicciones$class) * 100
paste("trainig_error=",trainig_error,"%")



######p40Ejemplo QDA billetes falsos
install.packages("mclust")
install.packages("knitr")
library(mclust)
library(knitr)
data(banknote)
#se recodifican las clases de la variable Status: verdadero = 0, falso = 1
levels(banknote$Status) 

levels(banknote$Status) <- c("falso","verdadero")
kable(head(banknote))

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = banknote, aes(x = Length, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = banknote, aes(x = Left, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = banknote, aes(x = Right, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p4 <- ggplot(data = banknote, aes(x = Bottom, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p5 <- ggplot(data = banknote, aes(x = Top, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, p3, p4, p5, nrow = 5, common.legend = TRUE, legend = "bottom")

pairs(x = banknote[,-1], col = c("firebrick", "green3")[banknote$Status],
      pch = 20)

# Representación mediante Histograma de cada variable para cada tipo de billete 
par(mfcol = c(2, 6))
for (k in 2:7) {
  j0 <- names(banknote)[k]
  x0 <- seq(min(banknote[, k]), max(banknote[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(banknote$Status)[i]
    x <- banknote[banknote$Status == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste( i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

# Representación de cuantiles normales de cada variable para cada tipo de billete
for (k in 2:7) {
  j0 <- names(banknote)[k]
  x0 <- seq(min(banknote[, k]), max(banknote[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(banknote$Status)[i]
    x <- banknote[banknote$Status == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    # los colores 2 y 3 son el rojo y verde
    qqline(x)
  }
}


#Contraste de normalidad Shapiro-Wilk para cada variable en cada tipo de billete
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(banknote, value.name = "valor")
datos_tidy %>% group_by(Status, variable) %>%
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5))


detach(package:mclust, unload = TRUE)
library(MVN)
outliers <- mvn(data = banknote[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")


royston_test <- mvn(data = banknote[,-1], mvnTest = "royston", multivariatePlot = "qq")

hz_test <- mvn(data = banknote[,-1], mvnTest = "hz")
hz_test$multivariateNormality

##Los datos no siguen una distribución normal multivariante, lo que tiene implicaciones 
##directas en la precisión del QDA.


library(biotools)
boxM(data = banknote[, -1], grouping = banknote[, 1])

##p47 El test Box’s M muestra mucha evidencia de que la matriz de covarianza no es constante en 
##todos los grupos, esta condición hace que el QDA sea más adecuado.

library(MASS)
modelo_qda <- qda(formula = Status ~ ., data = banknote, prior = c(0.01, 0.99))
modelo_qda


predicciones <- predict(object = modelo_qda, newdata = banknote)
table(banknote$Status, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(banknote$Status != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")


library(klaR)
partimat(formula = Status ~ ., data = banknote, prior = c(0.01, 0.99),
         method = "qda", prec = 200,
         image.colors = c("darkgoldenrod1", "skyblue2"),
         col.mean = "firebrick", nplots.vert = 4)



