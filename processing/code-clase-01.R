---
  # Titulo: "Script practica 1"
  # Autores: "Equipo docente Estad√???sticia Multivariada 2020"
  # Fecha: "1 de junio de 2020"
---
  # ---- Parte 0: Ordenar nuestra carpeta ---- 

# Les sugerimos siempre que antes de comenzar cualquier trabajo destinen una carpeta de directorio para poder ir 
# guardando sus avances y evitar perdidas. 
# Para ello utilizamos las teclas: "Ctrl + shift + h" simultaneamente y seleccionan su carpeta. 
# Tambien pueden hacerlo mediante este comando: 

# setwd("C:/Users/Nombre usuario/Lugar de la carpeta/Nombre de la carpeta")
setwd("C:/Users/ANDREAS/Desktop/Pr·ctica 1 EMV")

# Tengan en cuenta que la ruta de la carpeta deberan cambiarla por la ruta a la carpeta que ustedes vayan a utilizar.

# ---- Parte 1: Cargar librerias a utilizar---- 

# Los packages o librerias son un conjunto de herramientas que realizan funciones especeficas 
# o conjuntos ("familias") de funciones. Como ya se habran dado cuenta, estos paquetes son bastantes y 
# siempre hay actualizaciones, por lo que estar atentos a ellos y probarlos es provechoso. 

# El comando basico que utlizamos para instalar estos paquetes es "install.packages()". Este comando trae aquel 
# paquete desde la web y lo instala en nuestro software. Asi, tenemos nuestra propia biblioteca la cual nos 
# sera util a medida que realizamos nuestra sintaxis. 
# Para activar el paquete instalado utilizamos el comando "library()". 

# Para simplificar el trabajo en la sintaxis y tambien para mantener mayor orden a medida que desarrollamos 
# nuestro trabajo, existe el package "pacman" el cual es una libreria que nos permite instalar otras librerias 
# con mayor orden y en una sola funcion, es decir, realiza tanto la instalacion como la activacion.

# Dado que estamos recien comenzando y no sabemos si tenemos instalado el paquete "pacman" (Rstudio tiene memoria)
# instalaremos este paquete de la forma tradicional. 

#Instalar paquete pacman
install.packages("pacman")

#Activar paquete pacman
library(pacman)

# Empleamos el comando pacman para cargar las librerias a utilizar 
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer)

# Los paquetes que vamos a utilizar cumplen las siguientes funciones: 
# dplyr: ajuste general de datos
# sjmisc: descripcion y exploracion de base de datos
# car: principalmente la funcion recode para recodificar/agrupar valores de variable
# stargazer: para tabla descriptiva

# ---- Parte 2: Cargar datos a utilizar ----

# Previo a nuestro trabajo es recomendable realizar estos comandos:

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notacion cientifica

# En el ejemplo vamos a procesar variables de meritocracia y estatus (objetivo y subjetivo) 
# utilizando los datos de la encuesta ELSOC .
# Las bases de datos que usemos pueden ser "llamadas" como archivos locales (descargadas en nuestro equipo) o en linea.
# Para este caso utilizaremos un archivo en linea que viene en formato RData: ELSOC_W01_v3.10.RData.

# Cargamos nuestra base de datos desde la web 
load(url("https://multivariada.netlify.com/assignment/data/original/ELSOC_W01_v3.10.RData"))


# Como pueden observar en nuestra pestana "Enviroment" se agrego nuestra base con un nombre de objeto "elsoc_2016"

# Revisamos las dimensiones de la base
dim(elsoc_2016) # Este comando nos dice: 1) la cantidad de observaciones/filas/casos (tiene todos esos nombres) que en el caso de las encuestas suelen referirse a individuos y 2) La canidad de columnas/variables, por ejemplo edad, sexo etc.

# Veamos la base
View(elsoc_2016)


# ---- Parte 3: Seleccion de variables a utilizar ---- 

# Esta parte consiste en crear un subset reducido de datos que contenga solo las variables de interes. Para ello,
# debemos identificar el nombre de las variables que registran la informacion o items del instrumento. 
# Esto se puede realizar mediante el libro de codigos de la base, o bien, utilizando la funcion: "find_var" que nos
# entrega el nombre de la variable que fue registrada en la base de datos al buscar con un concepto asociado (no es recomendable).

find_var(data = elsoc_2016,"esfuerzo")

# Fijense que el output de este comando nos dice tres cosas. La primera es la ubicacion de la columna en la base, siendo la numero 158 de izquierda
# a derecha. La segunda es el nombre de la variable ("c18_09"). Es este nombre el cual utilizamos cuando estamos desarrollando codigos. La tercera 
# es la etiqueta de esta variable. En el caso de una encuesta se suele etiquetar con su pregunta correspondiente del cuestionario. OJO: No todas
# las bases de datos tienen etiquetadas sus variables (siempre es mejor cerciorarse).

# Luego, podemos crear nuestro subset de datos procesados. Para esto utilizamos la funcion "select()" del paquete "dplyr".
# El objeto en el cual almacenaremos este subset lo llamaremos "proc_elsoc". OJO: los nombres de los objetos quedan a libertad de nosotros, lo importante es que no contengan espacios, tildes, ni
#ningun caracter especial.

proc_elsoc <- elsoc_2016 %>% select(c18_09, # percepcion meritocracia esfuerzo
                                    c18_10, # percepcion meritocracia talento
                                    d01_01, # estatus social subjetivo
                                    m01,    # nivel educacional
                                    m0_sexo,# sexo
                                    m0_edad)# edad

# Antes de seguir, expliquemos un poco que hemos hechos aca. Podemos ver este comando (y muchos parecidos) a partir del "asignador". Esta es la
# flecha "<-" que se√±ala algo asi como "todo lo que este a la derecha de la flecha asignalo al objeto que esta a la izquierda de la flecha".
# "Objetos" en R es el termino que usamos para denominar a todo tipo de data con el que trabajemos, un objeto puede ser desde un numero hasta
# complejas ecuaciones.

# Comprobar que este objeto creado tenga nuestras variables 
names(proc_elsoc)

# Mediante el comando get_label obtenemos el atributo label de las variables. "Label" se refiere a etiquetas de la base de datos
sjlabelled::get_label(proc_elsoc)

# ---- Parte 4: procesamiento de variables ---- 

# El flujo de trabajo es el siguiente: 

#a. Descriptivo general
#b. Recodificacion: de casos perdidos y otros valores (en caso necesario)
#c. Etiquetado: cambio de nombres de variables y valores (en caso necesario)
#d. Otros ajustes

# --- 4.1 Variable Perpecion de meritocracia ---- 
# En nuestra base de datos, las variables que dan cuenta del funcionamiento de la meritocracia en Chile son: 

# [c18_09]: "Grado de acuerdo: Las personas son recompensadas por sus esfuerzos" (1 = 
# Totalmente en desacuerdo; 5 = Totalmente de acuerdo)

# [c18_10]: "Grado de acuerdo: Las personas son recompensadas por su inteligencia" (1 = 
# Totalmente en desacuerdo; 5 = Totalmente de acuerdo)

# ---- A. Descriptivo  general ----

frq(proc_elsoc$c18_09) # Este comando nos permite conocer la frecuencia, porcentaje, porcentaje valido y el porcentaje acumulado
frq(proc_elsoc$c18_10) 

# Podemos ver que los NS/NR estan codificados como -999 y -888. Estos valores los recodificaremos.

# ---- B. Recodificacion ----

# Para recodificar utilizamos la funcion recode, de la libreria car. 

proc_elsoc$c18_09 <- recode(proc_elsoc$c18_09, "c(-888, -999)=NA")
proc_elsoc$c18_10 <- recode(proc_elsoc$c18_10, "c(-888,-999)=NA")

# Este comando de arriba es utilizado para recodificar las categorias de respuesta de una variable de nuestra
# base de datos. Vale decir, de nuestra base ("proc_elsoc") seleccionamos una variable con el signo "$" (por ejemplo
# la c18_09) y le recodificamos sus categorias de respuesta que identificamos problematicas (-888 y -999) para volvernos NA's.

# ---- C. Etiquetado ----

#Ahora, cambiaremos tanto los nombres de la variable como su etiqueta.

# Cambiar nombre
proc_elsoc <- proc_elsoc %>% rename("mesfuerzo"=c18_09, # meritocracia esfuerzo
                                    "mtalento" =c18_10) # meritocracia talento

# Este comando sigue la misma logica que los demas: a mi objeto proc_elsoc renombrale las variables c18_09 y c18_10 por "mesfuerzo" y "mtalento" 
# respectivamente. Esto en razon de hacerlo mas viable (sustantivo) para nuestro trabajo y para la presentacion posterior de los datos (importante).

# Cambiar etiqueta

get_label(proc_elsoc$mesfuerzo) # Visualizar
proc_elsoc$mesfuerzo <- set_label(x = proc_elsoc$mesfuerzo,label = "Recompensa: esfuerzo") # Renombrar

get_label(proc_elsoc$mtalento) # Visualizar
proc_elsoc$mtalento  <- set_label(x = proc_elsoc$mtalento, label = "Recompensa: talento") # Renombrar

# Como ya lo trabajamos un poco m√°s arriba, el comando "get_label" le dice a R "muestrame las etiquetas del objeto". En este caso le estamos pidiendo que nos muestre las etiquetas de la variable "mesfuerzo" perteneciente a la base "proc_elsoc".
# Por otro lado, el comando para renombar los etiquetas le dice a R algo asi como: "guardame en la variable "mesfuerzo" de la base "proc_elsoc" la etiqueta "Recompensa: esfuerzo" y en la variable "mtalento" de la misma base ponle la etiqueta "Recompensa: talento".


# ---- D. Otros ajustes ----

# Para este caso vamos a crear una variable que sea el promedio de las dos variables de meritocracia.
# Bien sabemos que son variables numericas, por tanto, un promedio entre ambas es un buen estadistico 
# para realizar ordenaciones y base para estimaciones. 

proc_elsoc$pmerit <- (proc_elsoc$mesfuerzo+proc_elsoc$mtalento)/2 # Creacion de la nueva variable llamada "pmerit"
summary(proc_elsoc$pmerit)

get_label(proc_elsoc$pmerit) # Visualizar su etiqueta. Vemos que todavia tiene la etiqueta de la variable "Recompensa: esfuerzo".

proc_elsoc$pmerit  <- set_label(x = proc_elsoc$pmerit, label = "Meritocracia promedio") # Cambio etiqueta

# ---- Revision final ----
# Como se senala en la guia, es bueno realizar un descriptivo para verificar nuestras variables procesadas y 
# percatarnos de cualquier problema. Recomendable utilizar el comando "frq".

frq(proc_elsoc$mesfuerzo)
frq(proc_elsoc$mtalento)
frq(proc_elsoc$pmerit)

# ---- 4.2 Educacion ----
# [m01] = "¬øCual es su nivel educacional? Indique el tipo de estudio actual (si estudia actualmente) o el ultimo tipo aprobado
# (si no estudia actualmente)".

# ---- A. Descriptivo general ----

frq(proc_elsoc$m01) # Sacamos la frecuencias y porcentajes. 

# ---- B. Recodificacion ----

# Casos perdidos
proc_elsoc$m01 <- recode(proc_elsoc$m01, "c(-888,-999)=NA")
# Valores. Ojo: Revisar tabla de referencia CINE 2011 en https://multivariada.netlify.app/assignment/01-code/ para la recodificacion.

proc_elsoc$m01 <- car::recode(proc_elsoc$m01, "c(1,2)=1; c(3)=2;c(4,5)=3;c(6,7)=4;c(8,9,10)=5")

# El comando de arriba nos permite hacer una recodificacion basada en la literatura/criterios estandarizados, juntando categorias. 
# Por ejemplo, Tecnico Superior incompleta y Tecnico Superior completa se califica como "4" ya que hacen referencia al nivel 5 de la escala CINE.

# Comprobemos
frq(proc_elsoc$m01)

# Se observa que los valores coinciden con la recodificacion (los casos se acumulan entre las categorias 1 y 5 que fueron las que codificamos), 
# pero las etiquetas ahora no coinciden; se soluciona en el siguiente paso.

# ---- C. Etiquetado ----

# Para re-etiquetar valores usamos la funcion "set_labels" de la libreria "sjlabelled"

proc_elsoc$m01 <- set_labels(proc_elsoc$m01,
                             labels=c( "Primaria incompleta menos"=1,
                                       "Primaria y secundaria baja"=2,
                                       "Secundaria alta"=3,
                                       "Terciaria ciclo corto"=4,
                                       "Terciaria y Postgrado"=5))

# Este comando le asigna nuevos nombres a cada categoria de nuestra variable. Por ejemplo al 1 le asigna el nombre "Primaria incompleta menos"
# Luego renombramos la variable con un nombre mas sustantivo o viable de utilizar. 

proc_elsoc <- rename(proc_elsoc,"edcine"=m01) # Renombrar la variable

#Ademas de cambiar el nombre, queremos cambiar la etiqueta de la variable:

get_label(proc_elsoc$edcine) #Visualizar
proc_elsoc$edcine <- set_label(x = proc_elsoc$edcine,label = "Educacion") # Cambiar

# ---- 4.3 Estatus subjetivo ----

# [d01_01] = "Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena" (0 = el nivel mas bajo; 10 = el nivel mas alto)

# ---- A. Descriptivo general ----
frq(proc_elsoc$d01_01)# Frecuencias y porcentajes
summary(proc_elsoc$d01_01) # Medidas de tendencia central

# ---- B. Recodificacion ----
proc_elsoc$d01_01 <- recode(proc_elsoc$d01_01, "c(-888,-999)=NA") # Convertir a NA

#---- c. Etiquetado ----

#Cambio de nombre de variable a etiqueta mas sustantiva o viable "ess" (estatus social subjetivo).
proc_elsoc <- proc_elsoc %>% rename("ess"=d01_01) # estatus social subjetivo

#Ademas de cambiar el nombre, queremos cambiar la etiqueta de la variable.
get_label(proc_elsoc$ess) # Visualizar
proc_elsoc$ess <- set_label(x = proc_elsoc$ess,label = "Estatus Social Subjetivo") # Cambiar

# ---- 4.4 Sexo ----

# [m0_sexo] = Indicar el sexo del entrevistado.

# ---- A. Descriptivo general ----
frq(proc_elsoc$m0_sexo) # Frecuencias y porcentajes

# ---- B. Recodificacion ----
proc_elsoc$m0_sexo <- car::recode(proc_elsoc$m0_sexo, "1=0;2=1")

# Si bien esta variable no tiene mayores problemas de codificacion (casos) y comprension (etiquetas), 
# convencionalmente se utiliza la nomenclatura donde los hombres tienen valor 0 y las mujeres valor 1. Por tanto, haremos ese cambio



# ---- C. Etiquetado ----
# Ahora cambiamos las etiquetas de acuerdo a la recodificacion anterior:

proc_elsoc$m0_sexo <- set_labels(proc_elsoc$m0_sexo,
                                 labels=c( "Hombre"=0,
                                           "Mujer"=1)) # Cambio de etiquetas

# Asimismo, cambiaremos el nombre de la variable a algo mas simple:

proc_elsoc <- rename(proc_elsoc,"sexo"=m0_sexo) # Cambiar nombre

# Ademas de cambiar el nombre, queremos cambiar la etiqueta de la variable.
get_label(proc_elsoc$sexo) # Visualizar
proc_elsoc$sexo <- set_label(x = proc_elsoc$sexo,label = "Sexo") # Cambiar

#Revisar con un nuevo descriptivo:
frq(proc_elsoc$sexo)

# ---- 4.5 Edad ----

# [m0_edad] = ¬øCual es su edad? (anos cumplidos). 

# ---- A. Descriptivo general ----
frq(proc_elsoc$m0_edad) # Frecuencias y porcentajes

# ---- B. Recodificacion ----

# No es necesario en este caso, la edad viene registrada (por lo general es asi).

# ---- C. Etiquetado ----
proc_elsoc <- rename(proc_elsoc,"edad"=m0_edad) # Cambiar nombre por algo mas simple

#Ademas de cambiar el nombre, queremos cambiar la etiqueta de la variable.

get_label(proc_elsoc$edad) # Visualizar
proc_elsoc$edad <- set_label(x = proc_elsoc$edad,label = "Edad") # Cambiar



# ---- Parte 5: generacion de base de datos procesada ---- 
#Antes de guardar la base procesada, revisamos nuevamente todas las variables con una tabla descriptiva general 
# mediante la funcion stargazer.

stargazer(proc_elsoc, type="text")
stargazer(proc_elsoc, type="html")


 stargazer(proc_elsoc, type="html",align=TRUE, type = "html", no.space = TRUE, 
title = "Table X", out = "path/fit.html")


# Guardar base de datos procesada: en carpeta local
# save(proc_elsoc,file = "[ruta hacia carpeta local/ELSOC_ess_merit2016.RData")

# Para este guardado debemos especificar la ruta de nuestra carpeta de directorio. Veamos un ejemplo.  

save(proc_elsoc,file = "C:/Users/ANDREAS/Desktop/Pr·ctica 1 EMV/ELSOC_ess_merit2016.RData")


#En este caso, seguimos una estructura de carpetas de datos, separando en una carpeta los datos originales, y en otra (proc) los datos procesados:
save(proc_elsoc,file = "content/assignment/data/proc/ELSOC_ess_merit2016.RDa")