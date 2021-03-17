# ---- Parte 0: Identificación y descripción general ---- 

---
# Título: "Práctica 2"
# Autores: "Ivankovic, Jaime, Lara, Leiva, Lovazzano, Martínez & Miranda"
# Fecha: "22 de mayo de 2020"
  ---
  
# El objetivo de la Práctica 2 del curso es desarrollar el segundo momento del procesamiento de datos, esto es, 
# el análisis como tal. Para ello solo tocaremos, en esta ocasión, hasta la descripción de variables (el contraste de hipótesis vendrá después).  
  
# Antes de comenzar fijamos nuestra carpeta de directorio de trabajo. Podemos utilizar el atajo "Ctrl + MAYUS + h" simultáneamente y seleccionan su carpeta. 
  # También pueden hacerlo mediante este comando: setwd("C:/Users/Nombreusuario/Lugarcarpeta/Nombrecarpeta"), pueden saber esta información dando click derecho en la carpeta seleccionada y luego en propiedades, allí encontrarán la dirección de la carpeta y su nombre (deben cambiar el sentido de los slash "/").
  
# ---- Parte 1: Librerías principales a utilizar en el análisis ---- 

# Cargamos los paquetes (packages) a utilizar mediante el comando "pacman" que revisamos en la práctica 1 
# Si no sabemos si tenemos instalado el "pacman" pueden utilizar la secuencia básica: 
# install.packages ("pacman")
# library (pacman)

# Procedemos a instalar y cargar los paquetes 

pacman::p_load(dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               devtools)

# ---- Parte 2: Datos (provienen de la práctica 1) ---- 

# Vamos a cargar la base de datos ELSOC_ess_merit2016.Rproc_elsoc, que generamos durante la práctica 1. Se puede llamar desde el directorio en que la guardaron dando la ruta completa, o también para esta práctica la podemos llamar directamente desde nuestro sitio web:

# Cargar base de datos y fijarnos en nuestro espacio de trabajo (Enviroment). 
load(url("https://multivariada.netlify.app/assignment/data/proc/ELSOC_ess_merit2016.RData")) 

# Exploramos los nombres de las variables ("names") y las dimensiones ("dim") de la base de datos, es decir, cuántas filas (casos) y columnas (variables) tiene. 

names(proc_elsoc) # Muestra los nombres de las variables de la base de datos

dim(proc_elsoc) # Dimensiones

# Recordando el contenido de cada variable preparada en la práctica 1:

# [merit] = Indice promedio de percepción de meritocracia.
# [ess] = Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena" (0 = el nivel mas bajo; 10 = el nivel mas alto)
# [edcine] = Nivel educacional(1 = Primaria incompleta menos, 2 = Primaria y secundaria baja, 3 = Secundaria alta, 4 = Terciaria ciclo corto, 5 = Terciaria y Postgrado)
# [sexo] = Sexo (O = Hombre; 1 = Mujer)
# [edad] = ¿Cuáles su edad? (años cumplidos)

# Recomendación: siempre traten de observar la base de datos manualmente, leer el nombre de sus variables, las etiquetas, las dimensiones, los NA's y el carcter de los datos (usando el comando "class") 

View(proc_elsoc)
class(proc_elsoc$mesfuerzo)
class(proc_elsoc$mtalento)
class(proc_elsoc$ess)
class(proc_elsoc$edcine)
class(proc_elsoc$sexo)
class(proc_elsoc$edad)
class(proc_elsoc$pmerit)

# ---- Parte 3: Descripción de variables ---- 

# Haciendo memoria, los resultados referidos a descripción de variables se presentan en dos momentos del reporte 
# de investigación:

# Primero en la sección de metodología, cuando se presentan las variables del estudio en una tabla descriptiva 
# de variables.

# Segundo en la sección de análisis, que en general comienza con una exploración de asociaciones entre variables,
# también conocido como análisis descriptivo.

# ---- Parte 3.1: Tabla descriptiva de variables para la sección metodológica ---- 

# Se presentan tres opciones para hacer la tabla descriptiva para variables en la sección metodológica con distintas librerías de R

# a. Tabla descriptiva con stargazer 

# La función stargazer (de la librería del mismo nombre) permitirá mostrar los principales 
# estadísticos descriptivos univariados de las variables: medidas de tendencia central (media), 
# de dispersión (desviación estándar) y posición (mínimo, máximo, percentiles).

stargazer(proc_elsoc,type = "text")

# Algunas observaciones sobre esta tabla: 

# La opción type="text" permite que podamos ver los resultados directamente en la consola, de manera bastante rudimentaria. 
# Con otras opciones que veremos más adelante se puede estilizar para su publicación (también aprenderemos cómo importar estas tablas).

# Una distinción relevante a considerar cuando se describen variables es si estas son categóricas o continuas. 
# La definición de si una variables es tratada como categórica o continua es algo que hace el/la autor/a del 
# reporte, sin embargo hay variables nominales como sexo que claramente corresponden a categóricas, y por lo tanto no corresponde hacer un promedio entre ambas. 
# Sin embargo, como esta variable está codificada 0 (hombre) y 1 (mujer), en este caso lo que indica el valor de la columna promedio (Mean=0.60) es la proporción de mujeres vs hombres. 
# En otras palabras, hay un 60% de mujeres y 40% de hombres en la muestra (ya que se toma como categoría de referencia a los hombres "0").

# b. Tabla descriptiva con descr, libería sjmisc 

sjmisc::descr(proc_elsoc)

# Como se puede observar, cuando se presentan los simbolos "::" lo que estamos haciendo es seleccionando una función específica dentro de la librería. En este caso es la forma librería::función (sjmisc::descr). 

# Luego, seleccionamos algunas columnas específicas con información más relevante con la opción show.
# Además, agregamos la función kable para obtener una tabla que luego sea fácilmente publicable en distintos formatos (a profundizar en una práctica posterior):

sjmisc::descr(proc_elsoc,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

# Tabla descriptiva con summarytools::dfSummary

# Esta opción nos ofrece una tabla aún más detallada, con gráficos para cada variable, 
# las frecuencias para cada valor, y las etiquetas de las variables, por lo que es muy recomendable.

# Se específica de la siguiente manera:

dfSummary(proc_elsoc, plain.ascii = FALSE)

# Sin embargo, es muy ancha para visualizarla bien en la consola de R,
# para una versión más definitiva y publicable utilizamos el "view": 

view(dfSummary(proc_elsoc, headings=FALSE))


# ---- Nota sobre los casos perdidos (NA's) ---- 

# Hasta ahora hemos mantenido los casos perdidos en la base de datos, ya que son importantes de reportar en la tabla general de variables. 
# Sin embargo, de aquí en adelante se recomienda trabajar solo con casos completos, es decir, sacar los casos perdidos. 
# El quitar los casos perdidos de una base de datos es muy simple con la función "na.omit", pero para tomar precauciones y asegurarse que funciona se recomienda el siguiente procedimiento:

# - respaldar la base de datos original en el espacio de trabajo (por si queremos en adelante realizar algún análisis referido a casos perdidos), la dejaremos con el nombre proc_elsoc_original.
# - contamos el número de casos con el comando dim
# - contamos el número de casos perdidos con sum(is.na(proc_elsoc))
# - borramos los casos perdidos con proc_elsoc <-na.omit(proc_elsoc)
# - contamos nuevamente con dim para asegurarnos que se borraron
# - y por temas de funcionamiento de R, al realizar la operación de sacar casos perdidos, 
# se borra toda la información de las etiquetas (labels), así que las recuperamos de la base original con el comando copy_labels, de la librería sjlabelled.


# Respaldo de base original y dimensiones 
proc_elsoc_original <-proc_elsoc
dim(proc_elsoc)

# Conteo de número de casos perdidos 
sum(is.na(proc_elsoc))

# Borramos los NA's y contamos nuevamente sus dimensiones 
proc_elsoc <-na.omit(proc_elsoc)
dim(proc_elsoc)

# Podemos ver que la cantidad de casos (nuestro N) se redujo a 2887

# Recuperamos las etiquetas (labels) 
proc_elsoc <-sjlabelled::copy_labels(proc_elsoc,proc_elsoc_original)


# ---- 3.2: Exploración de asociación entre variables ---- 


# Dado que las hipótesis de investigación corresponden a asociación entre variables, 
# antes de realizar el contraste de hipótesis se suele presentar un análisis descriptivio que explora las asociaciones entre variables.

# La forma de explorar las asociaciones entre variables dependen de la naturaleza de las variables que se asocian:

# - Variables categóricas: tabla de contingencia
# - Variable categórica y continua: tabla de promedios por cada categoría
# - Variables continuas: correlaciones.

# La inclusión de gráficos en el reporte queda a desición del/a autor/a. 
# La pregunta que orienta esta decisión es: ¿Me permite enriquecer la discusión de los resultados en relación a las hipótesis planteadas? 

# ---- Tablas de contingencia para variables categóricas ---- 

# Para tablas de contingencia categóricas utilizaremos la función sjt.xtab, de la librería sjPlot. 

sjt.xtab(proc_elsoc$edcine, proc_elsoc$sexo)

# Al ejecutar el comando, el resultado aparece automáticamente en el visor de RStudio. 
# A esta tabla podemos también agregar porcentajes de filas y/o columnas, según sea lo más relevante analizar. En general se recomienda agregar solo un porcentaje, de otra manera la tabla se satura de información.

sjt.xtab(proc_elsoc$edcine, proc_elsoc$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE)



# ---- Tablas de promedio de variable continua por una categórica ---- 

# Para este práctico vamos a explorar datos de nuestra variable de perceción de meritocracia "permit" por los niveles educacionales edcine. 

# Una forma rápida de explorar esto es mediante la función tapply, que nos entrega de manera simple el promedio de una variable por otra:

tapply(proc_elsoc$pmerit, proc_elsoc$edcine, mean)

# Aquí podemos ver en promedio de pmerit para cada uno de los 5 niveles de la variable educación edcine. 

# Si se estima conveniente este tipo de cruces se puede representar también en una tabla con más opciones de información y también de publicación. Para esto utilizaremos una función algo más compleja de la librería dplyr.dplyr Esta librería permite aplicar una serie de funciones concatenadas y enlazadas mediante el operador %>% ("pipeoperator"). El sentido de cada función aparece comentado abajo:

proc_elsoc %>% # se especifica la base de datos
  select(pmerit,edcine) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(edcine)) %>% # se agrupan por la variable categórica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(pmerit),SD=sd(pmerit)) %>% # se agregan las operaciones a presentar en la tabla
  kable(, format = "markdown") # se genera la tabla


# Esta asociación también se pude representar de manera más simple con un gráfico, en este caso de cajas o boxplot mediante la función 
# "plot_grpfrq" de sjPlot:
  
  
plot_grpfrq(proc_elsoc$pmerit,proc_elsoc$edcine,
              type = "box")

# Se puede apreciar que este gráfico debiese aperecer en la sección "Plots" en nuestra pestaña. 

# ---- Correlaciones (variables continuas) ---- 

# Algunas notas sobre correlación: 

# El coeficiente de correlación mide la fuerza y dirección de la relación lineal entre dos variables continuas. Esta puede ser: 

# - positiva: a medida que auemnta una, aumenta la otra (ej:estatura y edad)
# - negativa: a medida que una aumenta, disminuye la otra (ej: tiempo dedico al estudio y probabilidad de reprobar) 
# - neutra: no hay asociación entre variables 

# El rango de variación del coeficiente de correlación va desde -1 (correlación negativa perfecta) y 1 (correlación positiva perfecta). 

# En el coeficiente de correlación se analiza tanto su tamaño como su significación estadística. 

# Existen diferentes formas de cálculo del coeficiente de correlación (Spearmen, Kendall, Pearson). 


# En lo que sigue nos concentraremos en el coeficiente de correlación más utilizado que es el de Pearson, que se aplica cuando las variables son de naturaleza continua.


# ---- Tablas/matrices de correlación ---- 

# Las correlaciones entre variables se presentan en general en modo de matrices, es decir, las variables se presentan en las filas y las columnas y en las celdas donde se cruzan los pares de variables se muestra su coeficiente de correlación.

# En su forma simple en R se aplica la función cor a la base de datos, 
# y la guardamos en un objeto que le damos el nombre M (en este caso) para futuras operaciones (este nombre de objeto queda a elección del/a autor/a pero procurar que sean sencillos y recordables). 


M <- cor(proc_elsoc)
M


# Como se puede apreciar es un reporte simple pero poco estilizado. Para una versión más amable utilizamos la función "sjt.corr" : 

sjt.corr(proc_elsoc)


# Algunas observaciones sobre la matriz de correlaciones: 

# En esta matriz las variables están representadas en las filas y en las columnas. 

# Cada coeficiente exresa la correlación de una variable con otra. Por ejemplo,  
# la correlación entre la variable de recompensa por esfuerzo y recompensa por inteligencia es 0.698.

# La información de cada coeficiente se repite sobre y bajo la diagonal, ya que es el mismo par de variables pero en el orden alterno (como un "cruce").

# En la diagonal corresponde que todos los coeficientes sean 1, ya que la correlación de una variable consigo misma es perfectamente positiva. En esta tabla se omiten y aparece la diagonal vacía, ya que es información redundante.

# Por lo mismo, también se recomienda eliminar el triangulo superior de la tabla (redundante) de la siguiente manera:

sjt.corr(proc_elsoc,
         triangle = "lower")

# ---- Segunda forma de presentar matrices de correlaciones ----

# Otra forma, también bastante usada, de presentar matrices de correlaciones es de manera gráfica con la librería "corrplot", cuya función corrplot.mixed se aplica al objeto que generamos con la función "cor (M)":

corrplot.mixed(M)

# Este gráfico/matriz representa el grado de asociación entre variables mediante 
# el tamaño de los círculos e intensidad de colores, y el signo de la asociación se 
# representa con una gradiente de colores que va del azul (positivo) al rojo (negativo). 
# Bajo la diagonal aparecen los indices de correlación entre pares de variables.


# ---- Tercera forma de presentar matrices de correlaciones ----  

# También se puede representar una correlación entre dos variables en un gráfico de nube de puntos o "scatterplot": 

# Seleccionamos las variables que queremos correlacionar, para ello miramos sus nombres para recordar  
names(proc_elsoc)

# Scatterplot
plot_scatter(proc_elsoc, edad, ess)

# Aquí podemos observar que: 1) cada punto representa un caso, y 2) la forma de la nuve indica si la asociación es positiva negativa o neutra. 

# En el caso de nuestra nube de puntos entre edad y estatus social subjetivo, observamos que no hay asociación (lo que ya era indicado por su correlación de -0.07 observada en la matriz de correlaciones).


# ---- Nota final: Información de la sesión de R ----

# R y sus librerías tienen distintas versiones. Esto puede representar algunos problemas de compatibilidad entre usuarios, por ejemplo, dos personas que trabajan en el mismo proyecto pero con distintas versiones (librerías y/o de R), pueden tener ocasionalmente complicaciones. Por eso, una buena práctica es registrar al final del código la información de la sesión. Y como siempre en R, varias maneras de hacer esto. Vamos con la más genérica que es muy simple: 

sessionInfo()

# Acá vemos un listado de información muy completo, desde versión de R, sistema operativo, opciones de idioma local (LOCALE), y muchas librerías. Si optamos por esta versión de la información de la sesión, lo importante es fijarse en (a) version de R, y (b) de las librerías cargadas al principio, que aquí aparecen bajo "other attached packages".

# La segunda opción permite obtener información más precisa, con sessioninfo sessioninfo()(la única diferencia con la anterior en el nombre es que info es con minúscula sessioninfo). Con un poco más de especificaciones de sintaxis se pueden obtener directamente los puntos (a) y (b) mencionados anteriormente:

session_info("sessioninfo")$platform

package_info(pkgs = (.packages()), dependencies = FALSE)
