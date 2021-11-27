##########p1 OPTIMIZACION LINEAL

###P1 EJEMP
#Un pastelero dispone de 150 kg de harina, 22 kg de azúcar y 27.5 kg de mantequilla para elaborar
#dos tipos de pasteles (A y B). Cada caja de pasteles de tipo A requiere 3 kg de harina, 1 kg de
#azúcar y 1 kg de mantequilla y su venta le reporta un beneficio de 20 euros. Cada caja de pasteles
#de tipo B requiere 6 kg de harina, 0.5 kg de azúcar y 1 kg de mantequilla y su venta le reporta
#un beneficio de 30 euros.¿Cuántas cajas de cada tipo debe elaborar el pastelero de manera que se
#maximicen sus ganancias? (Se supone en principio que también puede elaborar cajas incompletas,
#es decir, que no se trata de un problema de programación entera.)

#####
install.packages("lpSolve")
library(lpSolve)
# Parametros del problema
coef <- c(20, 30)
A <- matrix(c(3, 1, 1, 6, 0.5, 1), ncol=2)
b <- c(150, 22, 27.5)
dir <- rep('<=', 3)
# Solucion
solucion <- lp('max', coef, A, dir, b)
solucion$objval
#
solucion$solution

#Esto significa que la producción óptima del pastelero es de 5 cajas de A y 22.5 cajas de B, con lo que tendrá
#el máximo beneficio posible de 775 euros.

