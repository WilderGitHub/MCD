cavendish<- read.table("Cavendish.txt", header=TRUE)
cavendish
summary(cavendish)
install.packages("RODBC")
library(RODBC)
venta=odbcConnectExcel2007(file.choose())
#venta=odbcConnectExcel(file.choose())
misdatos=sqlFetch(venta,'Sheet1')
odbcClose()
misdatos

data(iris)
str(iris)
hist(iris$Petal.Length)
install.packages("ggplot2")
library(ggplot2)
ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
ggplot(iris,aes(x=Sepal.Length, fill=Species))+geom_histogram()

######

### HACER REFERENCIA DE GRANTICA DE LOS GRAFICOS Y
### LA CONFIGURACION (aes)

#Ejemplo () sin añadir ningun grafico 
diamonds
str (diamonds)
ggplot(diamonds)#(solo introducimos referencia de los datos)
ggplot(diamonds, aes(x=carat))  #solo inicamos el eje X
ggplot(diamonds, aes(x=carat, y=price))#Especificamos ambos ejes X e Y, 
#que ahora serán fijos en todas las capas.
ggplot(diamonds, aes(x=carat, color=cut))#Cada categoría de la variable
# "cut" tendrá un color diferente,
#una vez le indiquemos la geometría.
ggplot(diamonds, aes(x=carat), color="steelblue")#Si desea tener el color, 
#el tamaño, etc, fijo (es decir, 
#no varía basándose en una variable del marco de
#datos), es necesario especificarlo fuera de los aes(), 
#como en este caso.
###las Capas(geom)(ver pag 8)
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth()


ggplot(diamonds) +
  geom_point(aes(x=carat, y=price, color=cut)) +
  geom_smooth(aes(x=carat, y=price, color=cut))

## con geom_smooth()

ggplot(diamonds) +
  geom_point(aes(x=carat, y=price, color=cut)) +
  geom_smooth(aes(x=carat, y=price))

## o mas sencilo
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  geom_smooth()

##Ahora vamos a hacer que la forma de los puntos cambie con la característica "color" del conjunto de
##datos.

ggplot(diamonds, aes(x=carat, y=price, color=cut, shape=color)) +
  geom_point()

#### Las etiquetas (labs):Las etiquetas ayudan a visualizar lo que queremos representar en un gráfico. Las más comunes son el título
##principal del gráfico, los títulos de los ejes y las leyendas

gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  labs(title="Diagrama de puntos", x="Tamaño del diamante", y="Precio")
gg


##El Tema (theme)
###El ajuste del tamaño de las etiquetas se puede hacer usando la función theme() configurando
###plot.title, axis.text.x y axis.text.y, que deben ser especificados dentro del element_text().
###el título de la leyenda debe establecer el nombre utilizando scale_color_discrete(),

gg1 <- gg +
  theme(plot.title=element_text(size=30, face="bold"),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20)) +
  scale_color_discrete(name="Corte del diamante")

### El aspecto (facet)
#facet_wrap(fórmula) toma una fórmula como argumento. Los elementos que coloquemos a la derecha
#corresponden a las columnas, y los que coloquemos a la izquierda definen las filas.
#Por ejemplo, si definimos las columnas por 'cut'.

gg1 +
  facet_wrap( ~ cut, ncol=3)




#Si definimos las filas por "color" y columnas "cut".

gg1 +
  facet_wrap(color ~ cut)
#En facet_wrap, las escalas de los ejes X e Y se fijan para acomodar todos los puntos de forma
#predeterminada. es posible hacer que las escalas sean libres haciendo que las gráficas parezcan distribuidas
#más uniformemente estableciendo el argumento scales = free.

gg1 +
  facet_wrap(color ~ cut, scales="free")

###La leyenda: eliminación y cambio de posición.

#Si introducimos theme(legend.position = "none"), se puede quitar la leyenda. Mediante theme(legend.position
#= "top") se puede mover la leyenda alrededor de la trama.
#legend.justification denota el punto de anclaje de la leyenda, es decir, el punto que se colocará en
#las coordenadas dadas por legend.position.

#Ejemplo sin leyenda:

p1 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position="none") +
  labs(title="legend.position='none'")
p1

#Leyenda arriba:
p2 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position="top") +
  labs(title="legend.position='top'")
p2

#Leyenda dentro del gráfico:
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  geom_smooth() +
  labs(title="legend.position='coords inside plot'") +
  theme(legend.justification=c(1,0), legend.position=c(1,0))


######   3 ##########
ggplot(diamonds,aes(x=price)) +
  geom_histogram(fill="white",color="black")

#Cambiamos la anchura de los intervalos dentro de la geometría con el argumento binwidth, al aumentar
#el número cada vez tenemos menos intervalos:

ggplot(diamonds,aes(x=price)) +
  geom_histogram(fill="white",color="black",binwidth=1000)

#Los histogramas representan la frecuencia absoluta, si queremos que aparezca la frecuencia relativa,
#debemos especificarlo mediante y = (..count..)/sum(..count..):

ggplot(diamonds,aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill="white",
                 color="black",
                 binwidth=1000)

##Diagramas de cajas30

p <- ggplot(diamonds,aes(cut,price))
p +
  geom_boxplot(aes(fill=cut))

#Podemos superponer los puntos correspondientes a las obervaciones añadiendo geom_point()

p +
  geom_boxplot(aes(fill=cut))+
  geom_point()

#En muchos casos es interesante considerar distintos factores. En los datos del precio de los diamantes
#podemos considerar la calidad del corte cut y el color del diamante color. Para ello, basta con añadir una
#instrucción a la estética del boxplot.

p +
  geom_boxplot(aes(fill=factor(color)))

#También podemos resaltar los outliers

p +
  geom_boxplot(aes(fill=factor(color)),
               outlier.colour = "red",
               outlier.size = 1.5)

########################


##ESTADISTICA DESCRIPTIVA
##Para leer este archivo con R utilizaremos los siguientes comandos:
##para importar archivos con extension csv
install.packages("readr")
library(readr)

sargos = read.table(file = "sargos.csv", sep = ";", dec = ",",header = TRUE)
sargos

##Podemos habilitar un \acceso directo" a las variables por su nombre utilizando la funcion:

attach(sargos)

str(sargos)##(str:acronimo de estructura)
isla  ##isla es un factor; ello significa que si pedimos a R 
##que nos muestre sus valores,nos los mostrara como alfanumericos:

unclass(isla)##la funcion unclass() vemos que internamente los valores de esta variable
##estan almacenados como numeros enteros:


########de nuestro ejemplo, la variable que indica si un pez esta parasitado o no, ptdo
########Podemos conseguir este efecto creando un nuevo factor a partir de esta variable, y asignando
########etiquetas a sus valores mediante la siguiente sintaxis

fptdo = factor(ptdo, levels = c(0, 1), labels = c("No Parasitado","Parasitado"))
fptdo

#######Crear variables o recodicar variables existentes?
#######Acabamos de ver como se crea un factor (fptdo) a partir de una variable existente (ptdo). Si
#######hubisemos utilizado la sintaxis:

ptdo = factor(ptdo, levels = c(0, 1), labels = c("No Parasitado","Parasitado"))

#######en lugar de crear un nuevo factor, habriamos recodicado la variable ptdo ya existente, que de
####esta forma quedara convertida directamente en factor (y habra perdido sus valores originales,
####en este caso 0 y 1)4. Podemos comprobarlo, por ejemplo, utilizando el comando unique(),
####que muestra los valores distintos que toma la variable:

unique(ptdo)

####Es mejor crear nuevas variables o recodicar las que ya existen? 
####Si somos principiantes en R lo mejor es crear nuevas variables;

####En este caso particular la recuperacion resulta sencilla, ya que los valores originales de ptdo
####siguen almacenados en el data.frame sargos (vinculado al entorno de trabajo actual mediante
####el comando attach). Si borramos la variable ptdo mediante:

rm(ptdo)

#####en realidad solo borramos la variable recodicada; la variable ptdo del data.frame original,
#####que permanecia en el entorno de trabajo vuelve a ser accesible:

unique(ptdo)


######Tablas de frecuencias y representaciones graficas.
######Variables categoricas o numericas discretas.

table(isla)
prop.table(table(isla))

table(larvas)

prop.table(table(larvas))

cumsum(table(larvas))####Para las frecuencias acumuladas utilizamos la 
####funcion cumsum():

cumsum(prop.table(table(larvas)))

####Podemos construir una tabla mas compacta para estas frecuencias del siguiente 
####modo:
tbl = table(larvas)
nlarvas = names(tbl)
fi = as.vector(tbl)
fri = as.vector(prop.table(tbl))
Fi = cumsum(fi)
Fri = cumsum(fri)
data.frame(nlarvas, fi, fri, Fi, Fri)

####Sugerencia: Si necesitaramos hacer frecuentemente tablas como esta, resulta conveniente
####definir una funcion en R para ello, que nos ahorre tener que escribir todas estas lineas cada
####vez. Esta funcion podria ser, por ejemplo:
tablaFrec = function(x) {
  tbl = table(x)
  categ = names(tbl)
  fi = as.vector(tbl)
  fri = as.vector(prop.table(tbl))
  Fi = cumsum(fi)
  Fri = cumsum(fri)
  tabla = data.frame(categ, fi, fri, Fi, Fri)
  names(tabla)[1] = deparse(substitute(x))
  return(tabla)
}
tablaFrec(larvas)

####A medida que vamos trabajando con R podemos ir construyendo nuestra coleccion de funciones
####utiles y guardarlas, por ejemplo, en el archivo MisFunciones.R. Para tenerlas disponibles
####cada vez que usemos R bastara con ejecutar al principio de nuestra sesion:

#source("MisFunciones.R")


##########Graficos: diagramas de barras y diagramas de sectores.
##########Diagramas de barras, que en R se obtienen con el comando barplot().
##########Diagramas de sectores, que en R se obtienen con el comando pie().

barplot(table(isla))
pie(table(isla))
barplot(prop.table(table(isla)))

######Mejorando la presentacion de los graficos.

isla = factor(isla, levels = c("HI", "LP", "LG", "TF",
                               "GC", "FV", "LZ"), ordered = TRUE)
par(cex.axis = 0.9, las = 1)
barplot(prop.table(table(isla)), main = "Ejemplares capturados por isla",
        names.arg = c("Hierro", "La\nPalma", "La \nGomera",
                      "Tenerife", "Gran \nCanaria", "Fuerte-\nventura",
                      "Lanza-\nrote"), col = terrain.colors(12))

##### mejorando diagrama circular

noms = c("Hierro", "La Palma", "La Gomera", "Tenerife",
         "Gran Canaria", "Fuerteventura", "Lanzarote")
pct = prop.table(table(isla)) * 100
etiquetas = paste(noms, " (", pct, "%)", sep = "")
pie(table(isla), col = terrain.colors(7), labels = etiquetas,
    main = "Captura por isla")

#####Tablas cruzadas para variables categoricas o numericas discretas.

table(fptdo, isla)

####Podemos a~nadir los totales por  las y columnas mediante addmargins:

addmargins(table(fptdo, isla))


######Las distintas tablas cruzadas de frecuencias relativas se obtienen
######utilizando prop.table():

prop.table(table(fptdo, isla))


###Frecuencias relativas por filas: basta anadir a la funcion prop.table() 
#el argumento margin=1. Aqui ademas redondeamos a tres decimales:
  
  round(prop.table(table(fptdo, isla), margin = 1), 3)

#####Frecuencias relativas por columnas: Igual que en el caso anterior, 
#pero utilizando el  argumento margin=2:
  
  round(prop.table(table(fptdo, isla), margin = 2), 3)

#####Presentacion grafica de tablas cruzadas.

#####Las tablas de frecuencias cruzadas pueden representarse gra##camente tambien mediante bar-plot().

barplot(prop.table(table(sexo, isla)), col = c("pink2",
                                               "cyan3"), beside = TRUE, legend.text = TRUE, names.arg = c("Hierro",
                                                                                                          "La\nPalma", "La \nGomera", "Tenerife", "Gran \nCanaria",
                                                                                                          "Fuerteven-\ntura", "Lanza-\nrote"), las = 2)


####Tablas de Frecuencias para variables continuas.

install.packages("agricolae")
library(agricolae)
table.freq(hist(long, plot = F))
hist(long, xlab = "longitud", ylab = "Frecuencia", freq = FALSE,
     main = "Longitudes observadas en la muestra", col = topo.colors(40))

###Poligonos de frecuencias. 
#par(mfrow = c(1, 2))
#tbl = data.frame(table.freq(hist(long, plot = FALSE)))
#plot(tbl$MC, tbl$fi, type = "b", col = "red", lwd = 3,
#   xlab = "Marca de Clase", ylab = "Frecuencia", sub = "(Longitud del sargo)",
#   main = "(Poligono de frecuencias absolutas)"
#plot(tbl$MC, tbl$Fi, type = "b", col = "darkgreen", lwd = 3,
#xlab = "Marca de Clase", ylab = "Frecuencia", sub = "(Longitud del sargo)",
#main = "(Poligono de frecuencias absolutas \nacumuladas)"

######Medidas de posicion.

quantile(long, probs = c(0.05, 0.25, 0.5, 0.75, 0.9,0.95))

####Medidas de tendencia central.
median(long)

moda = function(x) {
  tbl = table(x)
  m = which(tbl == max(tbl))
  return(names(m))
}

moda(isla)

###En el caso de variables continuas, podemos usar la siguiente funcion 
###para obtener el  intervalo modal

intModal = function(x) {
  tbl = hist(x, plot = FALSE)
  m = which(tbl$counts == max(tbl$counts))
  im = data.frame(tbl$breaks[m], tbl$breaks[m + 1])
  names(im) = c("Inf", "Sup")
  return(im)}

intModal(long)

####Medidas de Dispersion.

var(long)
sd(long)
sd(long)/mean(long)
sd(peso)/mean(peso)
range(long)
diff(range(long))
quantile(long, probs = c(0.25, 0.75), names = FALSE)
diff(quantile(long, probs = c(0.25, 0.75), names = FALSE))

###Medidas de forma.
install.packages("agricolae")
library(agricolae)
skewness(ldors)
skewness(phig)


####Coeficiente de apuntamiento (curtosis):
kurtosis(ldors)
kurtosis(phig)


####Diagrama de cajas y barras (boxplot)
boxplot(long, col = "orange", main = "longitud")


####Medidas de sntesis en subgrupos de la muestra.

by(long, sexo, mean)
####o de manera equivalente:
aggregate(long, by = list(sexo), mean)


####La presentacion de la tabla construida con el comando aggregate() mejora si:
####La variable (o variables, ya que pueden incluirse varias) a resumir se especi####ca como
####subconjunto (subset()) del conjunto de datos original.
####La variable (o variables, tambien podran incluirse varias) que de####ne los grupos se renombra dentro del comando list().


aggregate(subset(sargos, select = c(long, peso)), by = list(Sexo = sexo,
                                                            Isla = isla), mean)


####Para concluir esta seccion citemos que es posible utilizar la funcion boxplot() para hacer
####diagramas de cajas y barras segun subgrupos de la muestra.

boxplot(peso ~ sexo, main = "Peso", col = c("pink2",
                                            "cyan3"))

boxplot(peso ~ isla, main = "Peso", col = heat.colors(14))


####Asociacion entre variables continuas.
####Regresion lineal.(pag 42Tema 0)

lm(peso ~ long)

####El valor indicado como intercept es la ordenada en el origen b0,
plot(long, ldors, xlab = "Longitud total", ylab = "Distancia morro-aleta dorsal",
     main = "Regresion Longitud-Distancia a la aleta dorsal")
recta = lm(ldors ~ long)
abline(recta, col = "darkgreen", lwd = 2)


plot(long, ldors, xlab = "Longitud total", ylab = "Distancia morro-aleta dorsal",
     main = "Regresion Longitud-Distancia a la aleta dorsal",
     type = "n")


with(subset(sargos, sexo == "Hembra"), {
  points(long, ldors, col = "pink3", pch = 19)
  abline(lm(ldors ~ long), col = "pink3", lwd = 2)
})

with(subset(sargos, sexo == "Macho"), {
  points(long, ldors, col = "cyan4", pch = 19)
  abline(lm(ldors ~ long), col = "cyan4", lwd = 2)
})

legend("topleft", c("Hembra", "Macho"), col = c("pink3",
                                                "cyan4"), pch = 19, lty = 2, bty = "n")

####Si queremos obtener los valores numericos de las ecuaciones de ambas rectas bastara con
#########ejecutar:

lm(ldors ~ long, data = subset(sargos, sexo == "Hembra"))

lm(ldors ~ long, data = subset(sargos, sexo == "Macho"))


intModal = function(x) {
  tbl = hist(x, plot = FALSE)
  m = which(tbl$counts == max(tbl$counts))
  im = data.frame(tbl$breaks[m], tbl$breaks[m + 1])
  names(im) = c("Inf", "Sup")
  return(im)
}


cov(long, ldors)
cor(long, ldors)


cov(data.frame(long, ldors, lpect, peso))

cor(data.frame(long, ldors, lpect, peso))

by(data.frame(long, peso), sexo, cor)

