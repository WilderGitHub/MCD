install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library("readxl")
install.packages("modeest")
library(modeest)
install.packages("GGally")
library(GGally)
install.packages("BSDA")
library(BSDA)

### Pregunta 1 
###  importamos la base de datos   ###
bd<-read.csv("datos_p_18.csv", sep = ";")
summary(bd)

# Histogramas
miHistograma = function(x, de, a, pasos ){hist(x, breaks = seq(from=de, to=a, by=pasos))}
miHistograma(bd$`precio`,100,200,10)
miHistograma(bd$`ingreso`,50,250,10)
miHistograma(bd$`autos`,0,50,10)
miHistograma(bd$`ventas`,0,5,1)
# Boxplot
miBoxplot = function(bd,color, x){
  p <- ggplot(bd,aes(color,x))
  p +  geom_boxplot(aes(fill=color))   }
boxplot(bd$`precio`, main="precio")
boxplot(bd$`ingreso`,main="ingreso")
boxplot(bd$`autos`,main="autos")
boxplot(bd$`ventas`,main="ventas")

############  Pregunta 2 Estadisticos (media,mediana, moda, RIQ, CV)
medidas = function(x){
  Medida<-c("Media","Mediana","Moda","Cuartil 25", "Cuartil 75","IQR", "Coef. de variación")
  Valor <-c(mean(x), median(x),mfv1(x),quantile(x, p=c(.25, .75)),IQR(x),sd(x)/mean(x))
  tabla = data.frame(Medida,Valor)
  tabla }

medidas(bd$`precio`)
medidas(bd$`ingreso`)
medidas(bd$`autos`)
medidas(bd$`ventas`)

##  covarianza y el coeficiente de correlación 
asociacion = function(x,y){
  Medida<-c("Covarianza","Coef. de Correlación")
  Valor<-c(cov(x, y), cor(x,y))
  tabla = data.frame(Medida,Valor)
  tabla}
asociacion(bd$`ingreso`,bd$`precio`)
asociacion(bd$`ventas`,bd$`precio`)
ggpairs(bd,
        columns=c("precio","ingreso","autos","ventas"))

#### Pregunta 3 ########
# a) 1   
n<-10
p<-0.3
x<-5 #igual a 5
dbinom(x=x, size=n, prob=p)
# a 2)  P(4<=X<=10
x<-4
y<-10
sum(dbinom(x:y, size=n, prob=p))
#metodo alternativo
pbinom(y,size=n, prob=p,lower.tail =TRUE)- pbinom(x-1,size=n, prob=p,lower.tail =TRUE)
#grafico
plot(dbinom(x:y, size = n, prob = p), type = "h", lwd = 6,
     main = "Función de probabilidad binomial",
     ylab = "P(X = x)", xlab = "Número de éxitos")
# a 3)
x<-5 # menor o igual
pbinom(x,size=n, prob=p,lower.tail =TRUE)
sum(dbinom(0:x, size=n, prob=p))
# b) 1
x<-3
lambda<-3.52
dpois(x=x, lambda=lambda)
# b) 2
x<-3
y<-7
sum(dpois(x:y, lambda=lambda))
ppois(y,lambda=lambda,lower.tail =TRUE)-ppois(x-1,lambda=lambda,lower.tail =TRUE)
# b) 3
lambda<-3.52
x<-4
dpois(x=x, lambda=lambda)

###  Pregunta 4
intervalos<-function(nombre,bd,ene,media,desv){
  media<-mean(bd)
  ene<-length(bd)
  desv<-sd(bd)
  error.est <- desv/sqrt(ene)
  margen.error <- 2.576 * error.est # nivel de confianza de 99% 
  lim.inf <- media - margen.error
  lim.sup <- media + margen.error
  cat("Variable: ", nombre, ", limite inferior: ", lim.inf, ", limite superior: ",lim.sup)
  #metodo alternativo
  zsum.test(mean.x=media,sigma.x=desv, n.x=ene,conf.level=0.99)

}
intervalos("Precio",bd$`precio`)
intervalos("Ingreso",bd$`ingreso`)
intervalos("Autos",bd$`autos`)
intervalos("Ventas",bd$`ventas`)

###  Pregunta 5
#a)
precio<-150
media<-mean(bd$precio)
desv<-sd(bd$precio)
  probZ<-pnorm(precio,media,desv,lower.tail = FALSE)
  probMayor<-2*probZ
  probMayor
#a)
ingreso<-160
media<-mean(bd$ingreso)
desv<-sd(bd$ingreso)
probZ<-pnorm(ingreso,media,desv,lower.tail = FALSE)
prob<-2*probZ
prob  

# Pregunta 6
biseccion = function(f, xa, xb, tol){
  if( sign(f(xa)) == sign(f(xb)) ){ stop("f(xa) y f(xb) tienen el mismo signo") }
  a = xa; b = xb
  k = 0

  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  repeat{
    m = a + 0.5*(b-a)
    if( f(m)==0 ){ cat("Cero de f en [",xa,",",xb,"] es: ", m ) }
    if( sign(f(a)) != sign(f(m)) ){
      b = m
    } else { a = m }
    dx = (b-a)/2
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
      if( dx < tol ){
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx)
      break;
    }
  }
}

f = function(x) x^2-cos(x)-1
curve(f, -2,2); abline(h=0, v=0)
biseccion(f, 1, 2, 0.000001)
