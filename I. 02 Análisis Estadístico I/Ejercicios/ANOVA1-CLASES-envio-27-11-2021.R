##Ejemplo de tiempo de coagulación sanguínea
#La tabla 4.1 muestra los tiempos de coagulación para muestras de sangre extraídas de 24 animales.
#QUE recibiEron cuatro dietas diferentes A, B, C, áhd D. (Para ayudar al lector a concentrarse en
#Essentia1s, en este libro hemos ajustado los datos para que salgan los promedios
#para ser números enteros.) Estos datos se trazan en la Figura 4.1. Los animales fueron
#asignado aleatoriamente a las dietas. y las muestras de sangre fueron tomadas y analizadas en
#el orden aleatorio indicado por los superíndices entre corchetes en el Tabl
#Considere la pregunta "¿Hay evidencia que indique una diferencia real entre
los tiempos medios de coagulación para los cuatro diferentes dietaas?.

coagulation <- read.table("e:/BASEBLOOD3.txt", header=TRUE)
coagulation
#plot(coag ~ diet, data=coagulation)
g <- lm(coag ~ diet, coagulation)
summary(g)
###Concluimos del pequeño valor p para la estadística F que hay alguna diferencia entre los grupos? 
###El grupo A es el nivel de referencia y tiene una media de 61, los grupos B, C y D son 5, 7 y 0 segundos mayores en promedio. 
###Examine la matriz de diseño para comprender la codificación.


#model.matrix(g)

##Podemos ajustar el modelo sin un término de intercepción como en
gi <- lm(coag ~ diet -1, coagulation)
summary(gi)


####16.1.4	Diagnostics

qqnorm(g$res)
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals", main="Residual-Fitted plot")
plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals", main="Jittered plot")





#176 Diferecnica significativa de Tukey

#está diseñado para todas las comparaciones por pares y depende
#en la distribución del rango estudiado. Deje X1  i.d. N µ                 
#Cuando los tamaños de muestra Ji son muy desiguales, el HSD de Tukey puede ser demasiado conservador, pero en general son
#más estrecho que los producidos por el teorema de Scheff. Existen varios otros métodos para comparaciones múltiples.
# El método de Tukey tiende a ser más conservador que la mayoría porque toma el más pesimista
#enfoque basado en la máxima diferencia. No todas las diferencias serán tan grandes como el máximo, por lo que
#Algunos métodos competitivos aprovechan esto para obtener intervalos más ajustados.
#Para referencia futura, una forma más general para los intervalos de Tukey es
## formula pag176

#donde l es el número de niveles del factor en el que estamos haciendo comparaciones múltiples y d f es el
#grados de libertad para el error.
#Calculamos las bandas de Tukey HSD para los datos de la dieta. Primero necesitamos el valor crítico del estudiante
#distribución de rango.

qtukey(0.95,4,20)

#Y EL INTERVALO ES:

(3.96/sqrt(2))*2.37*sqrt(1/6+1/6)
c(2-3.83,2+3.83)
#Una forma conveniente de obtener todos los intervalos es

TukeyHSD(aov(coag ~ diet, coagulation))

#Las bandas basadas en Bonferroni habrían sido un poco más anchas:

qt(1-.05/12,20)*2.37*sqrt(1/6+1/6)




                                                                                               