##P181
### PRUEBA DE HIPOTESIS
##Prueba de hipótesis para LA MEDIAS de una población normal

#Ejemplo1
contenido <- c(502, 501, 497, 491, 496, 501, 502, 500, 489, 490)
install.packages("nortest")
library(nortest)
ad.test(contenido)
#Como el valor-P de la prueba Anderson-Darling es 20 % y mayor que el nivel de
#significancia del 5 %, se puede asumir que la muestra proviene de una población
#normal.

t.test(contenido, alternative='two.sided',
conf.level=0.95, mu=500)

#Prueba de hipótesis para LAMEDIA?? con muestras grandes
#Ejemplo2

xbarra <- 19500 # Datos del problema
desvia <- 3900 # Datos del problema
n <- 100 # Datos del problema
mu <- 20000 # Media de referencia
est <- (xbarra - mu) / (desvia / sqrt(n))
est # Para obtener el valor del estadístico
pnorm(est) # Para obtener el valor-P

#Prueba de hipótesis para la proporción P
#Ejemplo3
z <- (174/200 - 0.90) / sqrt(0.90 * (1 - 0.90) / 200)
z # Para obtener el valor del estadístico
pnorm(q=z, lower.tail=TRUE) # Para obtener el valor-P

#Prueba ?^2 de Pearson
#Ejemplo4
prop.test(x=174, n=200, p=0.9, alternative='less',
conf.level=0.95, correct=FALSE)

#Ejemplo5

binom.test(x=17, n=20, p=0.9, alternative="less")

paired=TRUE, conf.level=0.95)