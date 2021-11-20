####INTERVALO DE CONFIANZA



#EJEMPLO2



url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=T)
datos

datos2 <- read.delim("E:/datos2.txt", comment.char="#")
View(datos2)

hombres <- datos2[datos2$sexo=="Hombre", ]
mujeres <- datos2[datos2$sexo=="Mujer", ]

par(mfrow=c(2,2))
require(car) # Debe instalar antes el paquete car
qqPlot(hombres$altura, pch=19, las=1, main='QQplot',
xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')

hist(hombres$altura, las=1, xlab='Altura', ylab='Frecuencia',
main='Histograma para la altura de hombres')
qqPlot(mujeres$altura, pch=19, las=1, main='QQplot',
xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')

hist(mujeres$altura, las=1, xlab='Altura', ylab='Frecuencia',
main='Histograma para la altura de mujeres')

t.test(x=hombres$altura, y=mujeres$altura,
paired=FALSE, var.equal=FALSE,
conf.level = 0.95)$conf.int

#EJEMPLO3
Antes <- c(81, 87, 86, 82, 90, 86, 96, 73, 74, 75, 72, 80, 66, 72, 56, 82)
Despues <- c(78, 91, 78, 78, 84, 67, 92, 70,58, 62, 70, 58, 66, 60, 65, 73)
Diferencia <- Antes - Despues

par(mfrow=c(1,2))
require(car)
qqPlot(Diferencia, pch=19, main='QQplot para Diferencias', las=1,
xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')

plot(density(Diferencia), main='Densidad para Diferencias', las=1,
xlab='Diferencia de tiempo', ylab='Densidad')

t.test(x=Antes, y=Despues, paired=TRUE, conf.level=0.95)$conf.int


##Para construir intervalos de INTERVALO DE CONFIANZA PARA LA VARIANZA se usa la función
##var.test

install.packages("stests")
if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)

###PARA ACTUALIZAR PAQUETES
#inst <- packageStatus()$inst
#inst[inst$Status != "ok", c("Package", "Version", "Status")]

library(stests)
#stats::var.test() # Para usar la fución del paquete stats
#stests::var.test() # Para usar la fución del paquete stests
 
#EJEMPLO4

require(stests) # Para cargar el paquete
res <- stests::var.test(x=hombres$altura, conf.level=0.98)
res$conf.int

####Intervalo de confianza bilateral para la razón de
varianzas ??2 1/??2 2

#EJEMPLO5

var.test(x=hombres$altura, y=mujeres$altura,
conf.level=0.95)$conf.int


########Intervalo de confianza bilateral para la PROPORCION
#EJEMPLO 6

prop.test(x=275, n=500, conf.level=0.90)$conf.int

####Intervalo de confianza bilateral para la diferencia
##de proporciones ??1 - ??2


###EJEMPLO7
prop.test(x=c(75, 80), n=c(1500, 2000), conf.level=0.90)$conf.int