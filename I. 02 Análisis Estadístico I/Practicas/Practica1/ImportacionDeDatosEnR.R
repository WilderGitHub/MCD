# .	Para importar datos con extensi�n .xls y .xlsx (de Microsoft Excel)
#  M�todo 1. Utilizando una librer�a denominada "readxl"

install.packages("readxl")
library("readxl")
# para archivos con extensi�n xls
misDatos <- read_excel ("IBD.xls", sheet= "Data")
View(misDatos)
# para archivos con extensi�n xlsx
misDatos <- read_excel("archivo.xlsx", sheet= "Data")

# M�todo 2. Utilizando una librer�a denominada "readxl"
install.packages("xlsx")
library("xlsx")
Datos<-read.xlsx("IBD.xls", 1, header=TRUE)
View(Datos)

# M�todo 3. Utilizando la interfaz de R Studio

# .	Para importar datos con extensi�n .csv
library(readr) 
presidentes<-read.csv("presidents.csv")
View(presidentes)

# .	Para importar datos con extensi�n .txt
consumo<-read.table("consumo.txt", header = TRUE, sep = "", dec = ".")
View(consumo)
