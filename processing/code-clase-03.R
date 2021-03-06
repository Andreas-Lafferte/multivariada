# Codigo practica 3: Regresion simple.

# 1. Cargar librerias ----
pacman::p_load(stargazer, ggplot2, dplyr)

# 2. Cargar datos ----
## Desde internet 
datos <- read.csv("https://multivariada.netlify.app/slides/03-regsimple1/tacataca.txt", sep="")

## Desde computador local
datos <- read.csv("input/tacataca.txt", sep="")

# 3. Descriptivos ---- 
## Verificacion datos
View(datos)
sapply(datos, class)

## Tabla descriptiva
stargazer(datos, type = "text")
stargazer(datos %>% select(juegos_x,puntos_y) , type = "text")

# 4. Bivariados ----
#¿tiene relación la experiencia previa (juegos jugados previamente) con el desempeño actual 
# (puntos obtenidos)?

## Grafico
g=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point()
g

## Correlación
cor(datos$juegos_x,datos$puntos_y)

#¿Cuántos puntos (Y) se obtienen según la experiencia previa de juego (X)?: La media condicionada de Y según el valor de X
# La pregunta cambia.

## Grafico de medias condicionales del ejemplo
g2=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)
g2


# Residuos : observado - predicho

# 5. Modelo e hipotesis ----
## Calculo paso a paso de parametros del ejemplo

## Para estos calculos revisar explicacion en pagina web
datos$difx=datos$juegos_x-mean(datos$juegos_x)
datos$dify=datos$puntos_y-mean(datos$puntos_y)
#Creamos un vector para juegos_x y para puntos_y en funcion de sus medias

datos$difcru=datos$difx*datos$dify
datos$difx2=datos$difx^2
datos
sum(datos$difcru) #Suma de los productos o diferencia cruzada
sum(datos$difx2) #Suma de la diferencia del promedio de X al cuadrado

## Estimacion modelo de regresion simple
reg1 <-lm(puntos_y ~ juegos_x, data = datos) # la estructura de la funcion es: objeto <- lm(dependiente ~ independiente, data=datos)
reg1

stargazer(reg1, type = "text") # Tabla