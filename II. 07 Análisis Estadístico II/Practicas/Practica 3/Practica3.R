#########  Ejercicio 1 ################

install.packages("readxl")
install.packages("ggvis")
install.packages("party")
vuelos<-read.csv("vuelostodo.csv",sep = ";")
head(vuelos)
summary(vuelos)

str(vuelos)

vuelos$edad <- as.numeric(vuelos$edad)
vuelos$distancia <- as.numeric(vuelos$distancia)
vuelos$comida <- as.numeric(vuelos$comida)
vuelos$limpieza <- as.numeric(vuelos$limpieza)
vuelos$retraso <- as.numeric(vuelos$retraso)
vuelos$genero <- as.factor(vuelos$genero)
vuelos$satisfaccion <- as.factor(vuelos$satisfaccion)

var(vuelos$edad)
var(vuelos$distancia)
var(vuelos$comida)
var(vuelos$limpieza)
var(vuelos$retraso)

hist(vuelos$edad,main="Histograma de Edad ")
hist(vuelos$distancia,main="Histograma de Distancia ")
hist(vuelos$comida,main="Histograma de Comida")
hist(vuelos$retraso,main="Histograma de Retraso ")

plot(density(vuelos$distancia), main="Grafico de distancia")
table(vuelos$satisfaccion)
pie(table(vuelos$satisfaccion),col=rainbow(10))

cov(vuelos[,2:6])
cor(vuelos[,2:6])

library(ggvis)
vuelos%>%ggvis(~edad,~retraso,fill=~satisfaccion)%>%
layer_points()
vuelos%>%ggvis(~distancia,~retraso,fill=~comida)%>%
layer_points()


head(vuelos[,2:7])
library(party)
set.seed(1234)
ind<-sample(2,nrow(vuelos), replace=T,prob=c(0.7,0.3))

trainData<-vuelos[ind==1,]
testDAta<-vuelos[ind==2,]


myFormula<-comida~retraso+edad
vuelos_ctree<-ctree(myFormula,data=trainData)
table(predict(vuelos_ctree),trainData$comida)
plot(vuelos_ctree)





#########  Ejercicio 2 ################

install.packages("readxl")
install.packages("PerformanceAnalytics") 
install.packages("psych") 
install.packages("rela")
install.packages("factoextra")
library (readxl)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(rela)
library(factoextra)

localidades<-read_excel("localidades.xlsx")
head(localidades)

str(localidades)
missing(localidades)

localidades2=localidades[c(2:6)]


cor(localidades2)

corrplot(cor(localidades2))


cortest(cor(localidades2))

#Se rechaza H0 , por lo cual se acepta H1 lo que da a entender que las correlaciones son distintas.

#PRUEBA DE ESFERICIDAD DE BARLETT

cortest.bartlett(cor(localidades2),n=850)

#Pvalue es 0 por ello se acepta Ha , lo cual significa que existe correlación entre las 
#variables cuando usas cor(data).

#Prueba KMO
KMO(localidades2)

#Según el resultado se justifica la realizacion del PCA (ya que MSA>0.5).

scree(localidades2)

#Visualizamos en el grafico de sedimentacion la cantidad optima de componentes que pueden haber en 
#la data en este caso se observa que el numero de componentes son 3.

#Analisis paralelo
fa.parallel(localidades2,fa="pc")

#Se utilizara la funcion fa.parallel con lo cual podemos realziar gráficos de pantalla de datos o 
#matrices de correlación en comparación con matrices aleatorias.

comp_localidades<-prcomp(localidades2, scale=TRUE,center = TRUE)
comp_localidades

#Con la función prcomp podemos realiza un analisis de PCA en la matriz de datoS para posteriormente 
#pasarla a objeto y mediante la funcion scale se escalan los datos para que todos tengan un valor 
#igualitario y se centra la data para que se distribuya y organize de una mejor manera

summary(comp_localidades)

#Con esta funcion podemos apreciar algunas caracteristicas de los objetos de la matriz de componentes ,
#de los cuales se puede resaltar “proportion of variance” el cual nos muestra que tan alejada estan los datos de la media

localidadesA <- localidades[,2:5]
pca <- prcomp(localidadesA, scale = TRUE)
fviz_eig(pca)

plot(comp_localidades)


fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#Con esta grafica se peude apreciar el porcentaje de data que tiene cada variable

biplot(comp_localidades, scale=0.5)


#Se usa la funcion “biplot” y se escala a 0.5 para que el grafico esta mas centrado y las variables puedan tener la mayor 
#cantidad datos

comp_principal<-comp_localidades$x
comp_principal<-comp_principal[,1:3]
head(comp_principal)

#Se crea una nueva variable donde se almacenara las varables de los componentes creados anteriormente y se escojen los que pertenecientas 
#a las primera a la tercera variable y posteriormente se visualizan las 5 primeras filas

comp_principal

#mediante este condigo se puede observar todas las observaciones pertenecientes a la data con los 3 componentes anteriormete seleccionados


