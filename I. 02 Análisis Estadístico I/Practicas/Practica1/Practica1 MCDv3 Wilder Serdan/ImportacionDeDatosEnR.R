# .	Para importar datos con extensión .xls y .xlsx (de Microsoft Excel)
#  Método 1. Utilizando una librería denominada "readxl"

install.packages("readxl")
library("readxl")
# para archivos con extensión xls
misDatos <- read_excel ("IBD.xls", sheet= "Data")
View(misDatos)
# para archivos con extensión xlsx
misDatos <- read_excel("archivo.xlsx", sheet= "Data")

# Método 2. Utilizando una librería denominada "readxl"
install.packages("xlsx")
library("xlsx")
Datos<-read.xlsx("IBD.xls", 1, header=TRUE)
View(Datos)

# Método 3. Utilizando la interfaz de R Studio

# .	Para importar datos con extensión .csv
library(readr) 
presidentes<-read.csv("presidents.csv")
View(presidentes)

# .	Para importar datos con extensión .txt
consumo<-read.table("consumo.txt", header = TRUE, sep = "", dec = ".")
View(consumo)
