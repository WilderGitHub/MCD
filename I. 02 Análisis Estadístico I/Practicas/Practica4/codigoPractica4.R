install.packages("readxl")
library("readxl")
install.packages("multcompView")
library(multcompView)

cuero <- read_excel ("cuero.xlsx")
g <- lm(desgaste ~ tipo, cuero)
summary(g)
boxplot(desgaste~tipo, data=cuero)#model.matrix(g)
##Podemos ajustar el modelo sin un t�rmino de intercepci�n como en
#gi <- lm(coag ~ diet -1, coagulation)
#summary(gi)

#####   LSD      #####################

model<-aov(desgaste~tipo, data=cuero)
out <- LSD.test(model,"virus", p.adj="bonferroni")
plot(out)
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(cuero,LSD.test(desgaste,tipo,df,MSerror))
plot(out,variation="IQR")

###Tukey########

model=lm(cuero$desgaste ~ cuero$tipo )
ANOVA=aov(model)
ANOVA
TUKEY <- TukeyHSD(x=ANOVA, 'cuero$tipo', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")


## Ejercicio 3 ########

g=function(x) 0.5*(sin(x)-cos(x))
N=100
x0=0.001
tol=0.00000000001
curve(g(x), col = "blue", lty = 2, lwd = 2, xlim=c(-5,5),ylim=c(-5,5))

abline(h=0, col="red")
abline(v=0,col="red")

for(i in 1:N) {
  x = g(x0); e=abs(x-x0)
  print (c(i,x0,x))
  if (e < tol)
  {cat("converge en", i, "iteraciones. Raiz= ",x)
    return()
  }
  x0=x 
}

## Ejercicio 4 ########

g=function(x) (2*x^3-2)/(3*x^2-3)
N=100
x0=1.2
tol=0.00000000001
curve(g(x), col = "blue", lty = 2, lwd = 2, xlim=c(-15,15),ylim=c(-13,13))

abline(h=0, col="red")
abline(v=0,col="red")

for(i in 1:N) {
  x = g(x0); e=abs(x-x0)
  print (c(i,x0,x))
  if (e < tol)
    {cat("converge en", i, "iteraciones. Raiz= ",x)
  return()
    }
  x0=x 
  }

########## Ejercicio 5 ##############

biseccion = function(f, xa, xb, tol){
  if( sign(f(xa)) == sign(f(xb)) ){ stop("f(xa) y f(xb) tienen el mismo signo") }
  # a = min(xa,xb)
  # b = max(xa,xb)
  a = xa; b = xb
  k = 0
  #Par imprimir estado
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
    # imprimir estado
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    #until
    if( dx < tol ){
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx)
      break;
    }
  } #repeat
}

f = function(x) x^5-100*x^4+3995*x^3-79700*x^2+704004*x-3160075
curve(f, -2000,2000); abline(h=0, v=0)
biseccion(f, 17, 222, 177)

########## Ejercicio 6 ##############
f = function(x) x^5
curve(f, -2,2); abline(h=0, v=0)
biseccion(f, -0.2, 0.1, -0.2)

########## Ejercicio 7 ##############

newton1 = function(f, fp, x0, tol, maxiter){
  k = 0
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dx = abs(x1-x0)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanz� el m�ximo n�mero de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}

f = function(x) x^3-2*x-5
fp = function(x) 3*x^2-2
curve(f, -2,2); abline(h=0, v=0)
options(digits = 15)
newton1(f,fp, 0, 0.0000005, 10)

########## Ejercicio 8 ##############

f = function(x) x^5-7
fp = function(x) 5*x^4
curve(f, -2,2); abline(h=0, v=0)
options(digits = 15)
newton1(f,fp, -100, 0.000005, 1)
########## Ejercicio 9 ##############

f = function(x) x^5-5*x^4+5*x^3-6
fp = function(x) 5*x^4-20*x^3+15*x^2
curve(f, -2,2); abline(h=0, v=0)
options(digits = 15)
newton1(f,fp, -1000, 0.0005, 1)

