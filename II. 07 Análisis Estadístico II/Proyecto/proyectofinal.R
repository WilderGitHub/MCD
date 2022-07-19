############ PROYECTO FINAL ############
## instalamos las librerias necesarias
paquetes<-c("treemapify","GGally","useful","ggstream","ggfortify","readxl","ggplot2","tidyverse","dplyr","fpp2","tseries")
installed_packages <- paquetes %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {install.packages(packages[!installed_packages])}
invisible(lapply(paquetes, library, character.only = TRUE))

## leemos los datos
ine<-read_excel("ine.xlsx")
ine$FECHA <- as.Date(ine$FECHA, "%Y-%m-%d")
head(ine)
summary(ine)
str(ine)
destino<-"CHILE"
################# analisis mediante clustering ######
#agrupamos
inex<-aggregate(x = ine$VALOR,by = list(FECHA=ine$FECHA,DESTINO=ine$DESTINO),FUN = sum)  
inex=rename(inex,VALOR=x)
str(inex)
summary(inex)
##   función para filtrar
pais <- function(bd,destino,desde,hasta){
  bd %>%
  filter(DESTINO==destino) %>%
  filter(FECHA >=as.Date(desde)) %>%filter(FECHA<=as.Date(hasta))}
##   función para gráficos serie y promedio
grafico <- function(datos,titulo){
  dt<-aggregate(datos,FUN=mean)
  df<-fortify(datos)
  df2<-fortify(dt)
  df2$Index<-as.Date(as.character(df2$Index),format = "%Y")
  colors<-c("Exportaciones"="#56B4E9","Promedio"="red")
  ggplot(mapping = aes(x=Index,y=Data))+
    geom_line(data=fortify(df,melt=TRUE), aes(color="Exportaciones"))+
    geom_line(data=fortify(df2,melt=TRUE),aes(color="Promedio"))+
    labs(title=titulo,x="Periodo",y="Millones de $us", color="")+ scale_color_manual(values = colors)}
##   función para Streamgraph
stream <- function(datos,titulo){
  cols <- c("#875B52","#E76F51","#8FA078", "#228176", "#E9C46A", "#59804D", "#F4A261", "#264653", "#BB8865", "#A69B4E", "#65453E", "#E24D28", "#73835D", "#11403B", "#A07918", "#354D2E", "#EF7A1A", "#47829A", "#8E5E3E", "#6F6734", "#902C14", "#3F4833", "#7EDDD3", "#864109", "#274754")
  ggplot(datos, aes(x = FECHA, y = VALOR, fill = PRODUCTO)) +
    geom_stream() +scale_fill_manual(values = cols)+
    labs(title = titulo,subtitle = "(En millones de $us)",caption  = "Fuente: INE"
    )}
### StreamGraph
unpaisproductos<-pais(ine,destino,"2000-12-31","2022-12-31")
stream(unpaisproductos,paste("Productos exportados a",destino))
### Box plot
unpais<-pais(inex,destino,"2008-12-31","2019-12-31")
unpais <- ts(unpais$VALOR, frequency = 12, start = 2009)
boxplot(unpais~cycle(unpais))
## serie y promedio
grafico (unpais,paste("Exportaciones a ",destino))
## descomposición
descomposicion <-decompose(unpais,"multiplicative")
autoplot(descomposicion)


#########
# install.packages("treemapify")
#library(treemapify)
# install.packages("ggplot2")
#library(ggplot2)

# ggplot(df, aes(area = valor, fill = valor, label = grupo)) +
#   geom_treemap() +
#   geom_treemap_text(colour = c(rep("white", 2),
#                                1, rep("white", 6)),
#                     place = "centre", size = 15) +
#   scale_fill_viridis_c() 


## Test de no estacionariedad
adf.test(unpais)
# modelo
arima<-auto.arima(unpais)
arima
ggtsdiag(arima)
# predicción
prediccion<-forecast(arima,level = c(95),h=12)
autoplot(prediccion)
prediccion
################# analisis mediante clustering #######

gestion<-ine %>%
  filter(FECHA >=as.Date("2022-01-01")) %>%filter(FECHA<=as.Date("2022-12-31"))%>%
  group_by(PRODUCTO, DESTINO) %>% 
  summarize(VALOR = sum(VALOR))%>%
  pivot_wider(names_from = DESTINO,values_from = c(VALOR))
gestion[is.na(gestion)] = 0
gestion


summary(gestion)
gestion$PRODUCTO<-as.factor(gestion$PRODUCTO)

#ggpairs(gestion[,2:23], lower = list(continuous = "smooth"),
#        diag = list(continuous = "bar"), axisLabels = "none")

gestion[,2:23]<-scale(gestion[,2:23])
head(gestion)
##
distancias<-dist(gestion[,2:23])
distancias
##agrupamiento
agrupamiento<-hclust(distancias)


## k grupos
(grupos<-cutree(agrupamiento, k=3))
###
plot(agrupamiento,hang=-1,cex=0.7, labels=gestion$PRODUCTO, main="Cluster
de exportaciones por País destino")

rect.hclust(agrupamiento, k=3, border="red")

