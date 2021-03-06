##Ejemplo de tiempo de coagulaci�n sangu�nea
#La tabla 4.1 muestra los tiempos de coagulaci�n para muestras de sangre extra�das de 24 animales.
#QUE recibiEron cuatro dietas diferentes A, B, C, �hd D. (Para ayudar al lector a concentrarse en
#Essentia1s, en este libro hemos ajustado los datos para que salgan los promedios
#para ser n�meros enteros.) Estos datos se trazan en la Figura 4.1. Los animales fueron
#asignado aleatoriamente a las dietas. y las muestras de sangre fueron tomadas y analizadas en
#el orden aleatorio indicado por los super�ndices entre corchetes en el Tabl
#Considere la pregunta "�Hay evidencia que indique una diferencia real entre
los tiempos medios de coagulaci�n para los cuatro diferentes dietaas?.

coagulation <- read.table("e:/BASEBLOOD3.txt", header=TRUE)
coagulation
#plot(coag ~ diet, data=coagulation)
g <- lm(coag ~ diet, coagulation)
summary(g)
###Concluimos del peque�o valor p para la estad�stica F que hay alguna diferencia entre los grupos? 
###El grupo A es el nivel de referencia y tiene una media de 61, los grupos B, C y D son 5, 7 y 0 segundos mayores en promedio. 
###Examine la matriz de dise�o para comprender la codificaci�n.


#model.matrix(g)

##Podemos ajustar el modelo sin un t�rmino de intercepci�n como en
gi <- lm(coag ~ diet -1, coagulation)
summary(gi)


####16.1.4	Diagnostics

qqnorm(g$res)
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals", main="Residual-Fitted plot")
plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals", main="Jittered plot")





#176 Diferecnica significativa de Tukey

#est� dise�ado para todas las comparaciones por pares y depende
#en la distribuci�n del rango estudiado. Deje X1  i.d. N �                 
#Cuando los tama�os de muestra Ji son muy desiguales, el HSD de Tukey puede ser demasiado conservador, pero en general son
#m�s estrecho que los producidos por el teorema de Scheff. Existen varios otros m�todos para comparaciones m�ltiples.
# El m�todo de Tukey tiende a ser m�s conservador que la mayor�a porque toma el m�s pesimista
#enfoque basado en la m�xima diferencia. No todas las diferencias ser�n tan grandes como el m�ximo, por lo que
#Algunos m�todos competitivos aprovechan esto para obtener intervalos m�s ajustados.
#Para referencia futura, una forma m�s general para los intervalos de Tukey es
## formula pag176

#donde l es el n�mero de niveles del factor en el que estamos haciendo comparaciones m�ltiples y d f es el
#grados de libertad para el error.
#Calculamos las bandas de Tukey HSD para los datos de la dieta. Primero necesitamos el valor cr�tico del estudiante
#distribuci�n de rango.

qtukey(0.95,4,20)

#Y EL INTERVALO ES:

(3.96/sqrt(2))*2.37*sqrt(1/6+1/6)
c(2-3.83,2+3.83)
#Una forma conveniente de obtener todos los intervalos es

TukeyHSD(aov(coag ~ diet, coagulation))

#Las bandas basadas en Bonferroni habr�an sido un poco m�s anchas:

qt(1-.05/12,20)*2.37*sqrt(1/6+1/6)




                                                                                               