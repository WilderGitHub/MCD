#########REGRESION LINEAL SIMPLE-califica

install.packages("readr")
library(readr)

califica = read.table(file = "califica.csv", sep = ";", dec = ",",header = TRUE)
califica
summary(califica)
attach(califica)
plot(calime, califi)

###Estimacion de la recta de regresion.  lpect =BETA0 +BETA1 ldors + ERROR
recta = lm(califi ~ calime)
recta
plot(calime, califi)
abline(recta, col = "red", lty = 2, lwd = 2)
confint(recta)##(INTERVALO DE CONFIANZA dela recta)

##b) 
calific<- 13.2151+0.7646*85
calific
#####Contraste de la regresion.
summary(recta)

####Prediccion de valores individuales.
xpred = data.frame(calime = c(45, 55, 65))
predict(recta, xpred, interval = "prediction", level = 0.95)


###Prediccion del valor medio condicionado.
xpred = data.frame(calime = c(45, 55, 65))
predict(recta, xpred, interval = "confidence", level = 0.95)


#####Grafico de las bandas de confianza.

install.packages("UsingR")
require(UsingR)
simple.lm(calime, califi, show.ci = TRUE, conf.level = 0.95)

#plot(calime~califi,data=df)

#####Validacion del ajuste de la recta.
#####H0 : Los residuos siguen una distribucion normal

shapiro.test(residuals(recta))


####Inferencia sobre el coeficiente de correlacion.
cor(califi, calime)
cor.test(califi, calime)

##########analisis de varianza

anova(recta)

############analisis de resiudos
fit1 <- lm(califi ~ calime)
fit1
summary(fit1)

opar <- par(no.readonly = TRUE)
modelo <- lm(califi ~ calime)
par(mfrow = c(2,2))
plot(modelo)
