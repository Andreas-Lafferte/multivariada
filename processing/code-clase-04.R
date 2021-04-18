# Codigo practica 4: Regresion simple y ajuste.

# 1. Cargar librerias ----
pacman::p_load(stargazer, ggplot2, dplyr,webshot)

# 2. Cargar datos ----
## Desde internet 
datos <- read.csv("https://multivariada.netlify.app/slides/03-regsimple1/tacataca.txt", sep="")

## Desde computador local
datos <- read.csv("input/tacataca.txt", sep="")

# 3. Bivariados ----
g2=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
g2

ggsave("output/images/g2.png", g2)

## Para guardar como imagen generica en R 
png(file = "output/images/g2.png") # se abre un archivo vacio
g2  # se genera el grafico a guardar en el archivo
dev.off() # se cierra el archivo

## Residuos: observado - predicho
## Minimizar la suma cuadrada de los residuos = OLS

# 4. Modelo ---- 
reg1 <-lm(puntos_y ~juegos_x, data = datos)
reg1

stargazer(reg1, type="text")

sjPlot::tab_model(reg1, show.ci=FALSE)

## Grabar/ exportar tablas
stargazer(reg1, type="html",  out = "reg1.html")
webshot("reg1.html","reg1.png")

sjPlot::tab_model(reg1, show.ci=FALSE, file = "reg1_tab.html")
webshot("reg1_tab.html","reg1_tab.png")

# 5. Ajuste ----
## La R-cuadrada se define como: R2= SEC/STC =1 - SRC/STC. 
## R2 es el cociente de la variacion explicada entre la variacion total; por tanto, se interpreta como "la proporcion de la variacion muestral de Y que es explicada por X"

#summary(lm(puntos_y~juegos_x, data=datos))
#beta=0.5 intercepto=2.5

#Variable de valores predichos
datos$estimado<- (2.5 + datos$juegos_x*0.5)

# Alternativa por comando
datos$estimado <- predict(reg1)

#Estimamos el residuo
datos$residuo <- datos$puntos_y - datos$estimado

# Alternativa por comando
datos$residuo <- residuals(reg1)

datos %>% select(id, estimado, residuo)

# 5.1. Suma de cuadrados y R2 ----
## Suma total de cuadrados
ss_tot<- sum((datos$puntos_y-mean(datos$puntos_y))^2); ss_tot

## Suma de cuadrados de la regresion
ss_reg<-sum((datos$estimado-mean(datos$puntos_y))^2) ; ss_reg

## Suma residual al cuadrado
ss_err<- sum((datos$puntos_y - datos$estimado)^2); ss_err

## Calculo R2
#Opcion 1
ss_reg/ss_tot

#Opcion 2
1-ss_err/ss_tot

#por comando
summary(lm(puntos_y~juegos_x, data=datos))$r.squared

# 6. Visualizacion ----
library(ggplot2)

ggplot(datos, aes(x=juegos_x, y=puntos_y))+
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +#Pendiente de regresion
  geom_segment(aes(xend=juegos_x, yend=estimado), alpha = .2) + #Distancia entre estimados y datos en lineas
  geom_point() + #Capa 1
  geom_point(aes(y=estimado), shape =1) +
  theme_bw()

# tamaño y color 
ggplot(datos, aes(x=juegos_x, y=puntos_y))+
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +#Pendiente de regresion
  geom_segment(aes(xend=juegos_x, yend=estimado), alpha = .2) + #Distancia entre estimados y datos en lineas
  geom_point(aes(color = abs(residuo), size = abs(residuo))) + #tamaño de residuoes
  scale_color_continuous(low = "black", high = "red") + # color de los residuos
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y=estimado), shape =1) +
  theme_bw()

# 7. Regresion v/s correlacion ----
## Podemos llegar a beta con la correlacion
beta<-cor(datos$juegos_x,datos$puntos_y)*(sd(datos$puntos_y)/sd(datos$juegos_x));beta

reg1$coefficients[2] #llamamos al coeficiente beta (en posición 2) en el objeto reg1

## El coef de corr elevado al cuadrado es igual a R2 cuando las var son estandarizadas
cor(datos$juegos_x,datos$puntos_y)

(cor(datos$juegos_x,datos$puntos_y))^2

## La corr es bidireccional
cor(datos$juegos_x,datos$puntos_y) 
cor(datos$puntos_y,datos$juegos_x)

## La regresion es unidireccional
lm(datos$puntos_y~datos$juegos_x)$coefficients
lm(datos$juegos_x~datos$puntos_y)$coefficients