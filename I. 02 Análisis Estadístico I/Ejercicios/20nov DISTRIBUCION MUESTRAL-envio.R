
#####EJEMPLO1a
#CCNSOLUC PROBABILIDADES DE LA MEDIA MUESTRAL
#Sea una poblacion de tamaño N=15 con los datos de la variable aleatoria X siguientes: 1,1,2,2,2,4,4,5,5,5,6,6,7,7
#se extrae una muestra aleatoria n=36,con reemplazo(n>N. Hallar a)la media y la varianza poblacional.   
# b)la media y la varianza muestral. c)La probabilida de que la media muestral se encuentre entre 3.5 y 4.3
#d) en que proporcion esta la media muestral de la media verdadera menos y mas unq desviacion estandar?

##Solucion

x<-c(1,1,2,2,2,4,4,5,5,5,6,6,7,7)

#calculo de media y varianza
media_poblacional<-mean(x)#;media_poblacional
media_poblacional
#varianza poblacional

N<-length(x)
varianza_pob<-(sum(x^2)-(N*(media_poblacional)^2))/N
varianza_pob


#### media y varianza de la media muestral
# n=36

library(dplyr)
n<-36

set.seed(1)
muestra_cr<-sample(x,n,replace=TRUE)
muestra_cr

## media muestral

media_mcr<-mean(muestra_cr)
media_mcr


##varianza de la media muestral

var_media_muest_cr<-var(muestra_cr)/n
var_media_muest_cr

###la probabilidad P(3.5<mediamuestral<4.3)

probabilidad <-(Pr=round (pnorm(c(3.5,4.3), mean=media_mcr, sd=sqrt(var(muestra_cr)), lower.tail
=TRUE),4))
probabilidad

dif_probabilidad<-probabilidad[2]-probabilidad[1]
dif_probabilidad
######################
probabilidad <-(Pr=round (pnorm(c(-1/6,1/6), mean=0, sd=1, lower.tail=TRUE),4))
probabilidad
dif_probabilidad<-probabilidad[2]-probabilidad[1]
dif_probabilidad

#############################################
#EJEMPLO 1.1
#x~n(e;so)
E=4
SD = 1
#prob=P(a<z<b)
a=3
b=5
#TRUE= COLA INFERIOR O P(Z<K) 
P_sup=pnorm(b, E, SD, lower.tail=TRUE) 
P_inf= pnorm(a, E, SD, lower.tail=TRUE) 
prob=P_sup-P_inf
prob

#EJEMPLO 1.2

#x~N(E;SD)
E=4
SD=1
#SD,lower.ta¡l = TRUE-> P(X<x) 
#calculando el valor de x 
qnorm(0.9, E, SD,lower.tail = TRUE)

#EJEMPLO 4
#Ej4.1
#X~N(E;SD)
E=174.5
SD=6.9
#P(x_media<K)
distr_media=function(K,muestras){ 
  z=(K-E)/(SD/sqrt(muestras)) 
  p = pnorm(z,0,1,lower.tail=TRUE)
}
#P(A<x_med¡a<B) 
A= 172.5 
B=175.8 
n=25
ps=distr_media(B,n)
pi=distr_media(A,n)
prob=ps-pi
prob

##EJEJMPLO 5
#X~N(E;SD) 
E=2.75 
SD=1.299 
#P(x_media<K) 
distr_media=function(K,muestras){ 
  z=(K-E)/(SD/sqrt(muestras)) 
  p = pnorm(z,0,1,lower.tail=TRUE) 
} 
#P(x_media>A) 
A=3 
n=64 
prob=1-distr_media(A,n) 
prob 

#EJEJMPLO 7
#X~N(E;SD) 
E=71 
SD=7 
#P(suma_x<K) 
distr_sum = function(K,muestras){ 
  z=(K-(n*E))/(SD*sqrt(muestras)) 
  p = pnorm(z,0,1,lower.tail=TRUE) 
} 
#P(Sumas_de_x>A) 
A=300 
n=4 
prob=1-distr_sum(A,n) 
prob 

##DISTRIBUCION DE LA PROPORCION

##EJEMPLO 10
# p= proporcion de aceptacion 
p=0.40 
# p= proporcion de rechazo 
q=1-p 
# pSombrero  = pS 
#FUNCION PARA HALLAR: para P(pS<K) 
distr_prop =function(K,muestras){ 
 z=(K-p)/(sqrt(p*q/muestras)) 
 p = pnorm(z,0,1,lower.tail=TRUE) 
}   # FIN-FUNCION --------------- 
#RESOLVIENDO  P(pS>A) 
A=0.45 
n=384 
prob=1-distr_prop(A,n) 
prob


#EJEMPLO 12

# p= proporcion de prueba 
p=0.2 
# p= proporcion de rechazo a prueba 
q=1-p 
#introdusca los datos de: 
#pSombrero=pS -->  P(K1<pS<K2) = Resul 
K1=0.16 
K2=0.24 
Resul=0.8 
#_____________________HALLANDO n 
Resul2=(Resul+1)/2 
PP=qnorm(Resul2, 0, 1,lower.tail = TRUE) 
n=(PP*(sqrt(p*q))/(K2-p))^2 
n 

##### DISTRIBUCION DE DIFERNCIAD DE MEDIAS MUESTRALES

###EJEJMPLO13

#X1~N(E1;SD1)  #X2~N(E2;SD2) 
E1 = 100 
SD1= 14.142 
E2 = 85 
SD2= 12.247 
#P ( X1m-X2m < K ) <---FUNCION 
distr_diff_medias=function(K,muestra1,muestra2){ 
  z=(K-(E1-E2))/sqrt((SD1^2/muestra1)+(SD2^2/muestra2)) 
  p = pnorm(z,0,1,lower.tail=TRUE) 
} 
#P ( X1m-X2m > A ) 
A=20 
n1=20 
n2=25 
prob=1-distr_diff_medias(A,n1,n2) 
prob 

#EJEMPLO 15
#X1~N(E1;SD1)  #X2~N(E2;SD2) 
E1 = 100 
SD1= 1.23 
E2 = 100 
SD2= 1.37 
#P( X1m-X2m < K ) <---FUNCION 
distr_diff_medias=function(K,muestra1,muestra2){ 
  z=(K-(E1-E2))/sqrt((SD1^2/muestra1)+(SD2^2/muestra2)) 
  p = pnorm(z,0,1,lower.tail=TRUE) 
} 
n1=35 
n2=42 
#INCISO --A-- 
#P ( X1m-X2m > A ) 
A=0.45 
prob_a=1-distr_diff_medias(A,n1,n2) 
prob_a 
#INCISO --B-- 
#P ( A1<X1m-X2m < A2 ) 
A1=0.65 
A2=0.83 
prob_sup=distr_diff_medias(A2,n1,n2) 
prob_inf=distr_diff_medias(A1,n1,n2) 
prob_b=prob_sup-prob_inf 
prob_b 











  
