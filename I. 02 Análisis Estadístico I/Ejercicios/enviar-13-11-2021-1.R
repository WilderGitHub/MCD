install.packages(pkgs="distrEx")
library(distrEx)
#Combianciones Ejercicio 1
choose(35,3)

#probabilidad condicional Ejercicio 1
pB <- 60
pA_B <-  25
pAB <- pA_B/pB
pAB

#teorema de bayes Ejercicio 1
pH <- 0.6
pM_H <- 0.2
pV <- 0.4
pM_V <- 0.35
pM <- pH*pM_H+pV*pM_V
pM

#teorema de bayes Ejercicio 3
pP <- 0.25
pE_P <- 0.01
pS <- 0.35
pE_S <- 0.02
pT <- 0.4
pE_T <- 0.03
pp_E <- pP*pE_P/(pP*pE_P+pS*pE_S+pT*pE_T)
pp_E


#Ejercicio 1 hipergeométrica 
dhyper(2,4,10-4,5)

#Ejercicio 2 binomial 
dbinom(2,6,0.1)

#Ejercicio 3 binomial 
x=5
lambda=2
dpois(x,lambda)

#Variable aleatoria continua Ejercicio 1
f <- function(x) (3/4)*(-x^2+4*x-3)
integrate(f,lower =1.7,upper = 2.4)

#Variable aleatoria continua Ejercicio 2
f <- function(x) 100/x^2
P_X <- 1-integrate(f,lower = 100,upper = 200)$value
P_X

#Distribución Normal Ejercicio 1
punif(22, min=20, max=25, lower.tail=T)

#Distribución Normal Ejercicio 2
prob=0.898
media=650
desv=100
qnorm(prob,media,desv)

#Distribución Exponencial Ejercicio 1
x=400
lambda=1/360
1-pexp(x,lambda)

#Distribución Exponencial Ejercicio 2
x=4
rate=1/3
dexp(x,rate)


#Distribución Normal Ejercicio 1
x=500
media=650
desv=100
pnorm(x,media,desv)

#Distribución Normal Ejercicio 2
prob=0.898
media=650
desv=100
qnorm(prob,media,desv)

#Distribución Normal Ejercicio 3
x=5
n=7
prob=0.5
dnorm(x,n,prob)

#Distribución Normal Ejercicio 4
media=6
desv_est=1.1
pnorm(6.6, mean = media, sd = desv_est, lower.tail = TRUE)


#### script-EJEM-PROB-COND-DISTRIB -DISCRE-CONT
