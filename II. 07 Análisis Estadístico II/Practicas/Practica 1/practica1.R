### PRACTICA 1 ###########
##### Ejercicio 12-14
install.packages("rjson")
library("rjson")
json <- fromJSON(file="1214.json")
evaluaciones <- as.data.frame(json)
print(evaluaciones)

#graficos

library(GGally)
ggpairs(evaluaciones, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")


#install.packages('corrplot')
library(corrplot)
x <- cor(evaluaciones[,2:5])
corrplot(x, type="upper", order="hclust")


fit<-lm(evaluaciones$y~evaluaciones$x1+evaluaciones$x2+evaluaciones$x3+
  evaluaciones$x4)
fit
summary(fit)

confint(fit)

predict(fit,data.frame(lstat=(c(5,4))),interval="confidence")
predict(fit,data.frame(lstat=(c(5,4))),interval="prediction")



### PRACTICA 1 ###########
##### Ejercicio 12-55

library("rjson")
json <- fromJSON(file="1255.json")
blancura <- as.data.frame(json)
print(blancura)


#graficos

library(GGally)
ggpairs(blancura, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")


#install.packages('corrplot')
library(corrplot)
x <- cor(blancura[,1:7])
corrplot(x, type="upper", order="hclust")


fit<-lm(blancura$blancura~blancura$acido+blancura$cascada+blancura$agua
        +blancura$sulfuro+blancura$cloro+blancura$tela)
fit
summary(fit)

confint(fit)

predict(fit,data.frame(lstat=(c(5,4))),interval="confidence")
predict(fit,data.frame(lstat=(c(5,4))),interval="prediction")
