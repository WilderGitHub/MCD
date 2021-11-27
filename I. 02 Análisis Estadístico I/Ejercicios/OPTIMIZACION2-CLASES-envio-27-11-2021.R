#optimizacion
######105  metodo de punto fijo
#Iteración de Punto fijo.
#1e-9 = 0.000000001
puntofijo =function(g, x0, tol=1e-9, maxIteraciones=100){
k = 1
# iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
repeat{
  x1 = g(x0)
  dx = abs(x1 - x0)
  x0 = x1
  #Imprimir estado
  cat("x_", k, "= ", x1, "\n")
  k = k+1
  #until
  if(dx< tol|| k > maxIteraciones) break;
}
# Mensaje de salida
if( dx > tol ){
  cat("No hubo convergencia ")
  #return(NULL)
} else{
  cat("x* es aproximadamente ", x1, " con error menor que ", tol)
 }
}

#106 ejemplo##La ecuación (x+1)sen(x^2)¡x Æ 0 tiene dos soluciones en [0,1]. Una manera de expresar el problema de encontrar
#una solución de esta ecuación en términos de un problema de punto fijo es escribir la ecuación como 

g = function(x) (x+1)*sin(x^2)
puntofijo(g, 0.01, 1e-9) # maxIteraciones=100
puntofijo(g, 0.8, 1e-9)


####p110   El metodo de biseccion
#El método consiste en lo siguiente: Supongamos que en el intervalo [a,b] hay un cero de f . Calculamos el punto
#medio m Æ (a Åb)/2 del intervalo [a,b]. A continuación calculamos f (m). En caso de que f (m) sea igual a cero,
#ya hemos encontrado la solución buscada. En caso de que no lo sea, verificamos si f (m) tiene signo opuesto al
#de f (a). Se redefine el intervalo [a,b] como [a,m] o [m,b] según se haya determinado en cuál de estos intervalos
#ocurre un cambio de signo. A este nuevo intervalo se le aplica el mismo procedimiento y así, sucesivamente, iremos
#encerrando la solución en un intervalo cada vez más pequeño, hasta alcanzar la precisión deseada

###p113

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

## Pruebas
f = function(x) x-cos(x)
curve(f, -2,2); abline(h=0, v=0) #gráfico para decidir un intervalo
biseccion(f, 0.5, 0.8, 0.000001)


#####P118 METODO NEWTON

###P124 METODO DE NEWTON RAPHSON

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
cat("Se alcanzó el máximo número de iteraciones.\n")
cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
} else {
cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
## --- Pruebas
f = function(x) x-cos(x)
fp = function(x) 1+sin(x)
options(digits = 15)
newton1(f,fp, 0, 0.0000005, 10)


