# ---- Parte 0. Identificación y descripción general ---- 

---
# Título: "Práctica 4. Regresión simple 2"
# Autores: "Ivankovic, Jaime, Lara, Leiva, Lovazzano, Martínez & Miranda"
# Fecha: "4 de junio de 2020"
---
# Antes de comenzar fijamos nuestra carpeta de directorio de trabajo. Podemos utilizar el atajo "Ctrl + MAYUS + h" simultáneamente y seleccionan su carpeta. 
# También pueden hacerlo mediante este comando: setwd("C:/Users/Nombreusuario/Lugarcarpeta/Nombrecarpeta"), pueden saber esta información dando click derecho en 
# la carpeta seleccionada y luego en propiedades, allí encontrarán la dirección de la carpeta y su nombre (deben cambiar el sentido de los slash "/").  

# ---- Parte 1. Librerias ----
pacman::p_load(stargazer, ggplot2, dplyr,webshot)

# ---- Parte 2. Datos ----
## Usaremos los mismos datos de la practica 3

## Desde internet
datos <- read.csv("https://multivariada.netlify.app/slides/03-regsimple1/tacataca.txt", sep="")
datos

## Desde carpeta local (siempre y cuando la base esté en el mismo working directory)
datos <- read.csv("tacataca.txt", sep="")

## Recta de regresión preeliminar 
g2=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
g2

# ---- 2.1 Grabar/ exportar graficos ----
## Para graficos ggplot
ggsave("g2.png", g2) # donde g2 es el nombre asignado al objeto de nuestro grafico

## Para guardar como imagen generica en R 
png(file = "g2.png") # se abre un archivo vacio
g2  # se genera el grafico a guardar en el archivo
dev.off() # se cierra el archivo

## El grafico quedara grabado en el directorio de trabajo (ver arriba). Si se desea que se grabe en otra parte, dar la ruta completa hacia la carpeta correspondiente ("C:/[ruta-hacia-carpeta]/g2.png")

# ---- Parte 3. Residuos ----
## Recordemos que los residuos son la diferencia entre valor observado y el valor predicho (o estimado)
## Por tanto, la mejor recta de regresion es aquella que minimice al maximo los residuos (suma de residuos al cuadrado)
## Este proceso de estimar la mejor recta se llama Ordinary Least Squares (OLS)

# ---- 3.1 Modelo y calculo de parametros ---- 
reg1 <-lm(puntos_y ~juegos_x, data = datos)
reg1

## Tabla stargazer 
stargazer(reg1, type="text")

## Tabla más publicable con sjPlot
sjPlot::tab_model(reg1, show.ci=FALSE)

## Grabar / exportar tablas 
# Para stargazer
stargazer(reg1, type="html",  out = "reg1.html")
webshot("reg1.html","reg1.png")

# Para sjPLot
sjPlot::tab_model(reg1, show.ci=FALSE, file = "reg1_tab.html")
webshot("reg1_tab.html","reg1_tab.png")

# ---- Parte 4. Bondad de ajuste: residuos y R2 ---- 
## Dado que en la estimacion estadistica utilizamos las probabiliades, siempre conlleva un grado de error
## La precison de nuestro modelo se relaciona con el concepto de "bondad de ajuste", que significa en terminos siemples ¿qué tan bueno es mi modelo? ¿qué tan bien se ajusta a la realidad de los datos?
## La bondad de ajuste se evalua a partir del R2 (R cudrado)

## Si nos fijamos, la R2 es igual que elevar al cuadrado el coeficiente de correlacion muestral de Pearsons si las variables estan estandarizadas. 
## La R-cuadrada de la regresión, tambien llamada coeficiente de determinación, se define como: R2= SEC/STC =1 - SRC/STC. 
## Es decir; R2 es el cociente de la variacion explicada entre la variacion total; por tanto, se interpreta como "la proporcion de la variacion muestral de Y que es explicada por X". Se suele expresar en porcentajes (se multiplica por 100 el valor que otorga, que siempre va de 0 a 1)

#summary(lm(puntos_y~juegos_x, data=datos))
#beta=0.5 intercepto=2.5

#Variable de valores predichos
datos$estimado<- (2.5 + datos$juegos_x*0.5)

# Alternativa por comando
#datos$estimado <- predict(reg1)

#Estimamos el residuo
datos$residuo <- datos$puntos_y - datos$estimado

# Alternativa por comando
#datos$residuo <- residuals(reg1)

datos %>% select(id, estimado, residuo)

# ---- Parte 5. Suma de cuadrados y R2 ---- 
## Suma total de cuadrados: Es la suma de als diferencias del promedio de Y al cuadrado (asociado al concepto de varianza de Y)
ss_tot<- sum((datos$puntos_y-mean(datos$puntos_y))^2); ss_tot

## Suma de cuadrados de la regresion: se refiere a la suma de diferencias (al cuadrado) entre el valor estimado por el modelo de regresion y la media. Expresa cuanto de la varianza de Y alcanzamos a predecir con X
ss_reg<-sum((datos$estimado-mean(datos$puntos_y))^2) ; ss_reg

## Suma de residuos al cuadrado: al contrario de el calculo anterior, los residuos representan la parte de la varianza de Y que no alcanzamos a abarcar con nuestro modelo de regresion. Es decir, reprsentan el error en la prediccion (diferencia entre lo estimado por el modelo y el valor observado)
ss_err<- sum((datos$puntos_y - datos$estimado)^2);ss_err

## Calculo R-cuadrada 
#Opción 1
ss_reg/ss_tot
#Opción 2
1-ss_err/ss_tot
#por comando
summary(lm(puntos_y~juegos_x, data=datos))$r.squared

# ---- Parte 6. Visualizacion ---- 
#Visualizacion
library(ggplot2)

ggplot(datos, aes(x=juegos_x, y=puntos_y))+
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +#Pendiente de regresion
  geom_segment(aes(xend=juegos_x, yend=estimado), alpha = .2) + #Distancia entre estimados y datos en lineas
  geom_point() + #Capa 1
  geom_point(aes(y=estimado), shape =1) +
  theme_bw()

## Agregando colo y tamaño a los residuos
ggplot(datos, aes(x=juegos_x, y=puntos_y))+
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +#Pendiente de regresion
  geom_segment(aes(xend=juegos_x, yend=estimado), alpha = .2) + #Distancia entre estimados y datos en lineas
  geom_point(aes(color = abs(residuo), size = abs(residuo))) +
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y=estimado), shape =1) +
  theme_bw()

# ---- Parte 7. Coeficiente de regresion versus el coeficiente de correlacion ----
## Tanto el coeficiente de correlacion como el beta de regresion son medidas de la relacion entre X e Y.
beta<-cor(datos$juegos_x,datos$puntos_y)*(sd(datos$puntos_y)/sd(datos$juegos_x));beta

reg1$coefficients[2] #llamamos al coeficiente beta (en posición 2) en el objeto reg1

## Del mismo modo, existe una relacion entre el coeficiente de correlacion y el beta de regresion
#Correlación (Pearson) entre juegos_x y puntos_y (r)
cor(datos$juegos_x,datos$puntos_y)

#Correlación entre juegos_x y puntos_y al cuadrado.
(cor(datos$juegos_x,datos$puntos_y))^2

## La correlacion entre X e Y es la misma que entre Y e X, es decir, no importa el orden de los productos
cor(datos$juegos_x,datos$puntos_y)
cor(datos$puntos_y,datos$juegos_x)

## Pero en la regresion no. La regresion entre X e Y NO es la misma que entre Y e X
lm(datos$puntos_y~datos$juegos_x)$coefficients
lm(datos$juegos_x~datos$puntos_y)$coefficients
