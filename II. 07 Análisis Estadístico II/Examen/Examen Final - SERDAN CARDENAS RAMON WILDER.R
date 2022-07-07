# Pregunta 1.
library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,universitarios = `HS Grad`, heladas = Frost, area = Area, .data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)
datos

# a)	Analizar la relación entre variables
library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

install.packages('corrplot')
library(corrplot)
x <- cor(datos[,1:9])
corrplot(x, type="upper", order="hclust")

# b)	Generar un modelo de regresión lineal con todas las variables (hacer un summary y analizar)
modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo +
             asesinatos + universitarios + heladas + area + densidad_pobl,
             data = datos )
summary(modelo)
# c)	Selección de los mejores predictores (hacer un summary y analizar)
step(object = modelo, direction = "both", trace = 1)
mejormodelo <- (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datos))
summary(mejormodelo)

# d)	Realizar la validación de condiciones para la regresión múltiple lineal

library(ggplot2)
install.packages('gridExtra')
library(gridExtra)
plotHabitantes <- ggplot(data = datos, aes(habitantes, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plotAsesinatos <- ggplot(data = datos, aes(asesinatos, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plotUniversitarios <- ggplot(data = datos, aes(universitarios, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plotHeladas <- ggplot(data = datos, aes(heladas, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plotHabitantes, plotAsesinatos, plotUniversitarios, plotHeladas)

# e)	Determinar si cumple con la distribución normal de los residuos

qqnorm(modelo$residuals)
qqline(modelo$residuals)

# f)	Realizar el test de normalidad
shapiro.test(modelo$residuals) 

# g)	Realizar el test homocedasticidad

ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

# h)	Realizar el Análisis de Inflación de Varianza (VIF)

install.packages('car')
library(car)
vif(mejormodelo)

# 2)

# 3)

#read.delim(file, header = TRUE, sep = "\t", dec = ".", ...)
insectos<-read.delim("insectos.txt", header = TRUE, sep = "\t")
insectos
insectos$especie <- as.factor(insectos$especie)


# descripción de los datos

library(ggplot2)
install.packages('ggpubr')
library(ggpubr)

pata <- ggplot(data = insectos, aes(x = pata, color = especie,fill=especie)) +
  geom_histogram(position = "identity",alpha = 0.5) 
pata +scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
abdomen <- ggplot(data = insectos, aes(x = abdomen, color = especie, fill=especie)) +
  geom_histogram(position = "identity",alpha = 0.5)
abdomen+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
organo_sexual <- ggplot(data = insectos, aes(x = organo_sexual, color = especie,
                                             fill=especie)) +
  geom_histogram(position = "identity",alpha = 0.5)
organo_sexual+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Correlaciones

library(GGally)
ggpairs(insectos, lower = list(continuous = "smooth", ggplot2::aes(colour=especie)),
        diag = list(continuous = "bar"), axisLabels = "none")

data(insectos)
ggpairs(insectos, columns = 2:4, ggplot2::aes(colour=especie, alpha=0.5)) 

# LDA 
install.packages('MASS')
library(MASS)
modeloDiscriminante <- lda(formula = especie ~ pata + abdomen + organo_sexual,
                  data = insectos)

modeloDiscriminante

# predicción

nuevoInput <- data.frame(pata = 180, abdomen = 130,
                                   organo_sexual = 40)
predict(object = modeloDiscriminante, newdata = nuevoInput)


# analisis gráfico


install.packages('scatterplot3d')
library(scatterplot3d)
scatterplot3d(insectos.pata, insectos.abdomen, insectos.organo_sexual,
              color = c("firebrick", "green3")[insectos.especie], pch = 19,
              grid = TRUE, xlab = "pata", ylab = "abdomen",
              zlab = "organo sexual", angle = 65, cex.axis = 0.6)

legend("topleft",
              bty = "n", cex = 0.8,
              title = "Especie",
              c("a", "b"), fill = c("firebrick", "green3"))


colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(insectos$especie)]
scatterplot3d(insectos[,2:4], pch = 16, color=colors)


p <- plot_ly(insectos, x=~insectos$abdomen, y=~insectos$pata, 
             z=~insectos$organo_sexual, color=~especie) %>%
  add_markers(size=1) 
print(p)
# 4)

# 4a) El modelo inicial

install.packages('faraway')
library(faraway)
data(meatspec)
meatspec

modelo <- lm(fat ~ ., data = meatspec)
summary(modelo)

# 4b)

SCE <- mean((modelo$fitted.values - meatspec$fat)^2)
SCE

# 4c)

modeloSegunStepwise <- step(object = modelo, trace = FALSE)
# Cantidad de predictores
length(modeloSegunStepwise$coefficients)

#4d)
install.packages('pls')
library(pls)
set.seed(2222)
modelo1 <- pcr(formula = fat ~ ., data = meatspec, scale. = TRUE,
                  validation = "CV")
modeloCROSS <- MSEP(modelo1, estimate = "CV")
which.min(modeloCROSS$val)

#4e)
testMSE <- mean((modelo1$fitted.values - meatspec$fat)^2)
testMSE

#4f)

plot(modeloCROSS$val, main = "MSE  y componentes", type = "l",
     ylab = "MSE",
     col = "red", xlab = "Componentes")
