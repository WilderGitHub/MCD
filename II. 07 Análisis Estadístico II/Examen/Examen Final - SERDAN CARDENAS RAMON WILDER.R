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

