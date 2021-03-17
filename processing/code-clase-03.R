# ---- Parte 0: Identificación y descripción general ---- 

---
# Título: "Práctica 3"
# Autores: "Ivankovic, Jaime, Lara, Leiva, Lovazzano, Martínez & Miranda"
# Fecha: "28 de mayo de 2020"
---
  
# El objetivo de la Práctica 3 del curso es  desarrollar ejercicios iniciales de regresión simple, 
# que fueron presentados en la clase respectiva. El ejemplo a utilizar es del libro 
# de Darlington & Hayes cap. 2 (The simple regression model).
  
# Antes de comenzar fijamos nuestra carpeta de directorio de trabajo. Podemos utilizar el atajo "Ctrl + MAYUS + h" simultáneamente y seleccionan su carpeta. 
# También pueden hacerlo mediante este comando: setwd("C:/Users/Nombreusuario/Lugarcarpeta/Nombrecarpeta"), pueden saber esta información dando click derecho en la carpeta seleccionada y luego en propiedades, allí encontrarán la dirección de la carpeta y su nombre (deben cambiar el sentido de los slash "/").
  
# ---- Parte 1: Librerías ----

# Verificamos que tengamos cargado el paquete "pacman". Luego procedemos a la instalación de las demás librerías. 

install.packages("pacman")
library(pacman)

# Aplicamos pacman 
pacman::p_load(stargazer, ggplot2, dplyr, sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               devtools)

# ---- Parte 2: Datos ----

# Los datos que vamos a utilizar se basan en el ejemplo del texto señalado. Estos corresónden a 
# un ejemplo ficticio de 23 casos (individuos) y sus datos en dos variables relacionadas 
# con un juego (originalmente de mini-golf en el texto de referencia, pero pensemos en un ejemplo más 
# cercano; de taca-taca). 

# Las dos variables de esta base de datos son el número de veces que se ha jugado antes (juegos_x) y 
# el número de goles o puntos ganados (puntos_y). El archivo de datos es tacataca.txt.

# Para cargar nuestra base de datos creamos un objeto mediante el asignador " <- ". En esta ocasión lo llamaremos "datos" 

# Dos formas de cargar la base de datos 

# Desde una base guardada en la computadora 
# datos <- read.csv("( ...ruta hacia el archivo ...)/tacataca.txt", sep="")

datos <- read.csv("C:/Users/ANDREAS/Desktop/Práctica 3 EMV/tacataca.txt", sep="")

# Desde la web 

datos <- read.csv("https://multivariada.netlify.app/slides/03-regsimple1/tacataca.txt", sep="")

# OJO: Como es un archivo de texto simple (txt), los cargamos con la función read.csv, para datos 
# guardados en texto simple separados por coma. Como en el caso de nuestros datos la separación 
# es por espacios en lugar de comas, agregamos esta información con la instrucción sep="" 

# ---- Parte 3: Verificación y descriptivos ---- 

# Verificamos que se hayan cargado los datos. Esto se puede hacer manual pinchando el objeto creado en el
# en Eviroment, o bien con el comando view 

View(datos)

# Verificamos las dimensiones y nombres de las variables. En este caso no poseen etiquetas (labels).
names(datos)
dim(datos)

# Poseemos 3 variables (columnas): 
# id: número único que identifica a cada sujeto
# juegos_x: número de veces que ha jugado previamente
# puntos_y: numero de puntos que obtuvo en el juego actual

# Realizamos una tabla descriptiva para la base de datos. Podemos usar las diferentes opciones vistas
# en el práctico 2, también es recomendable sacar una frecuencia si es necesario o la tabla no lo reporta.

stargazer(datos, type = "text")

# En la tabla vemos los estadísticos básicos de las variables juegos y puntos, y además aparece 
# la variable id, que es el identificador y por lo tanto no tiene sentido que salga en la tabla. 
# Para corregir, seleccionamos las variables de interés de datos con el operador pipe operator %>% 
# Este operador permite unir distintas funciones en una misma línea de código, y es muy utilizado por 
# librerías de manejo de datos como dplyr. Por ejemplo, ahora la instrucción es "de la base de datos datos" >%> "selecciona solo las columnas juegos y puntos":
  
stargazer(datos %>% select(juegos_x,puntos_y) , type = "text")

# ---- Parte 4: Análisis de datos ----

# ---- 4.1: Experiencia en juegos y puntuación ---- 

# La pregunta que nos hacemos para este ejercicio práctico es: ¿tiene relación la experiencia previa (juegos jugados previamente) con el desempeño actual (puntos obtenidos)?
# En la investigación sociológica esto debe ser un aspecto central en su objeto de investigación. 
# Debido a que la pregunta inicial de la estadística bivariada es si existe o no relación entre variables,
# procedemos a: 1) realizar un scatter ploto para conocer la forma de distribución y ajuste de los datos, y
# 2) realizar una correlación de Pearsons para este caso puesto que son dos variables continuas. 

# Veamos un gráfico de nube de puntos / scatter de ambas variables. Para eso, 
# primero cargamos la librería ggplotde R. Recordar que hay que instalarla primero si es que no se ha hecho
# previamente con install.packages("ggplot"). También agregamos ggplotly para incorporar elementos 
# interactivos en el gráfico:

g=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point()
g

# Vemos el objeto "g" y encontramos una posible asociación positiva que podemos corroborar con la función."cor".

cor(datos$juegos_x,datos$puntos_y)

# Tenemos una correlación positiva (dirección de la relación) y de un tamaño de efecto grande 
# (magnitud de la relación), para ciencias sociales. Es decir, existe una asociación positiva entre ambas
# variables: a medida que aumenta la experiencia en juegos, aumentan también los puntos obtenidos en 
# el partido de taca taca. Ahora bien, ¿cómo se relaciona más específcamente la experiencia en juegos con los puntos obtenidos posteriormente?
# Vale decir; ¿Cómo se relacionan las varianzas entre la experiencias en juegos con los puntos obtenidos posteriormente?

# ---- 4.2: Medias condicionales ----

# Antes de avanzar desde la correlación al método de regresión es importante conocer el concepto de media
# condicional.

#Como sabemos el promedio de Y (puntos) es 4. Es decir, si conocemos a algún individuo que pertence al 
# grupo de "datos", sabemos que su puntaje se encuentra probablemente cercano a 4. 
# ¿Podemos mejorar nuestra estimación utilizando el puntaje de X? Como lo conocemos, si el sujeto nos 
# dice que ha jugado antes 6 veces, dada la información que conocemos probablemente vamos a estimar un 
# puntaje superior de puntos, tal vez más cercano a 6.

# Lo que estamos haciendo es utilizar la información que conocemos de X para dar una estimación de Y, 
# que sea más precisa que el promedio bruto.



# Mirando el gráfico de nube de puntos, sabemos que tres personas han jugado antes una vez, pero una de 
# ellas tuvo 2 puntos, otra 3 y otra 4. Con estos datos podemos calcular la media de Y para X=1, 
# que sería igual a 3. En otras palabras, la media condicional de Y cuando X=3 es 1. Con esto, uno podría
# calcular la media condicional para cada punto de X y hacer una estimación más precisa de Y. Sin embargo,
# este proceso todavía no nos permite generalizar más eficientemte la relación entre X e Y.

# ¿Cuántos puntos (Y) se obtienen según la experiencia previa de juego (X)? Esta pregunta nos conduce 
# al cálculo de una recta que atraviese los puntos y que generalice la relación entre X e Y. 

# En otras palabras es preguntarse: ¿En la medida que aumenta mi experiencia previa de juego (cantidad de veces)
# también aumenta la cantidad de puntos promedios obtenidos? ¿Si juego más veces, probablemente podré obtener más puntos en el taca taca? 

g2=ggplot(datos, aes(x=juegos_x, y=puntos_y)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
g2

# Con este gráfico más legible estamos frente a una asociación lineal entre las variables, sin embargo, aún no responde nuestra pregunta,
# no abarca toda la variabilidad de los datos. Esto nos lleva a los residuos. 

# ---- 4.3: Residuos ---- 

# Los residuos son aquellos casos o valores que quedna alejados de la recta, es decir, son los outlires 
# que no ajustan su varianza con el promedio de los datos. Son la diferencia entre el valor predicho (recta) y el valor observado (caso).

# Por ejemplo, para el sujeto cuya experiencia es haber jugado 1 vez y luego gana 3 puntos, esta línea 
# predice exáctamente su puntaje basada en su experiencia. Sin embargo, el sujeto que ha jugado 
# 3 veces y saca 6 puntos se encuentra más lejos de la línea y por lo tanto esta línea o 
# "modelo predictivo" no representa tan bien su puntaje. A esto se refieren los residuos, que es la 
# diferencia entre el valor predicho Y gorro (o) y el observado Y , siendo los valores predichos de Y los que
# pasan por la recta a la altura de cada valor de X. Por lo tanto, la mejor recta será aquella que 
# minimice al máximo los residuos.

# El sentido de la recta que resume de mejor manera la relación entre dos variables es que minimice la suma de todos los residuos. ¿Cómo realizar este procedimiento?

# Para realizar la suma de los residuos estos se elevan al cuadrado, lo que se denomina Suma de 
# residuos al cuadrado o SS residual Se eleva al cuadrado ya que como hay residuos positivos y negativos,
# unos cancelarían a otros y la suma seía 0, tal como sucede en la formula de la varianza.

# De la infinita cantidad de rectas que se pueden trazar, siempre hay una que tiene un valor menor de SS residual.
# Este procedimiento es el que da nombre al proceso de estimación: mínimos (residuos) cuadrados ordinarios, o OLS (Ordinary Least Squares).


# ---- Parte 5: Modelo de regresión y cálculo de parámetros ---- 

#El modelo de regresión se representa con una ecuación de la recta, o recta de regresión. 
# Esta recta representa los valores predichos para Y según los distintos valores de X:

#  Y(estimada) = b0 + b1X 

# Simbolos: 
# Y gorro es el valor estimado/predicho de
# b0 es el intercepto de la recta (el valor de Y cuando X es 0)
# b1 es el coeficiente de regresión, que nos dice cuánto aumenta Y por cada punto que aumenta X (pendiente)
  
# ---- Cálculo de los parámetros del modelo de regresión ----

# b1, comunmente llamado el beta de regresión, se obtiene de la siguiente manera: 
 
# b1 = Cov (XY)/ Var X 

# Esto nos dice: qué parte de la covaración que hay entre X e Y se relciona con (la varianza de) X.

# Como sabemos, la base para todos estos cálculos es el valor de cada variable menos su promedio. Vamos a crear un 
# vector en nuestra base de datos difx= x - x(promedio) y dify= y - y(promedio).
  
datos$difx=datos$juegos_x-mean(datos$juegos_x)
datos$dify=datos$puntos_y-mean(datos$puntos_y)

# Y ahora con esto podemos obtener la diferencia de productos cruzados 
# dif_cru= (x - xpromedio) * (y - ypromedio), así como la diferencia de X de su promedio al cuadrado SSx= (x - xpromedio)^2

datos$difcru=datos$difx*datos$dify
datos$difx2=datos$difx^2
datos

# Con esto podemos obtener la suma de productos cruzados y la suma de cuadrados de X

sum(datos$difcru)

sum(datos$difx2)

# Aplicando la fórmula podemos obetner que b1 = 0.5

# Así, podemos oetener el valor de b0:

# b0 = Y(promedio) - b1X(promedio)
# b0= 4 - (3*0.5) = 2.5

# Completamos la ecuación de la regresión: 

# Y(gorro) = 2.5 + 0.5X

# Esto nos permite estimar el valor de Y (o su media condicional) basado en el puntaje X.
# Por ejemplo, cuál es el valor estimado de Y dado X = 5 ?  

# ---- Estimación del modelo de regresión simple ---- 

# La función para estimar regresión en R es lm (linear model). Su forma general es:

# objeto=lm(dependiente ~ independiente, data=datos)

# Apliquemos una regresión simple a nuestro práctica: 

reg1 <-lm(puntos_y ~ juegos_x, data = datos)

reg1

# Podemos observar el corte del eje Y (b0) que es 2.5 y el valor del beta de regresión (b1) que es 0.5

# Podemos tener un output en un formato más apropiado utilizando la librería stargazer

stargazer(reg1, type = "text")