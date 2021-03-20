# Codigo practica 1: Preparación de datos en R.

# 1. Cargar librerías ----
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer)

# 2. Cargar datos ----
## Ajustar espacio de trabajo
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación cientifica

## Desde internet 
load(url("https://multivariada.netlify.com/assignment/data/original/ELSOC_W01_v3.10.RData"))

## Chequeo basico de la base de datos
dim(elsoc_2016) # dimension de la base
View(elsoc_2016)

# 3. Seleccion de variables ---- 
## Identificar variables de interes 
find_var(data = elsoc_2016,"esfuerzo")

## Seleccion de variables 
proc_elsoc <- elsoc_2016 %>% select(c18_09, # percepcipn meritocracia esfuerzo
                                    c18_10, # percepcion meritocracia talento
                                    d01_01, # estatus social subjetivo
                                    m01,    # nivel educacional
                                    m0_sexo,# sexo
                                    m0_edad)# edad


## Comprobar
names(proc_elsoc)

## Obtener etiquetas de las variables 
sjlabelled::get_label(proc_elsoc)

# 4. Procesamiento de variables ---- 

# El flujo de trabajo: 

#a. Descriptivo general
#b. Recodificacion: de casos perdidos y otros valores (en caso necesario)
#c. Etiquetado: cambio de nombres de variables y valores (en caso necesario)
#d. Otros ajustes

## Descriptivos
frq(proc_elsoc$c18_09) 
frq(proc_elsoc$c18_10) 
frq(proc_elsoc$m01)
frq(proc_elsoc$d01_01)
frq(proc_elsoc$m0_sexo)
frq(proc_elsoc$m0_edad)

## Recodificacion
proc_elsoc$c18_09 <- recode(proc_elsoc$c18_09, "c(-888, -999)=NA") # casos perdidos
proc_elsoc$c18_10 <- recode(proc_elsoc$c18_10, "c(-888,-999)=NA")
proc_elsoc$m01 <- recode(proc_elsoc$m01, "c(-888,-999)=NA") 
proc_elsoc$d01_01 <- recode(proc_elsoc$d01_01, "c(-888,-999)=NA")

proc_elsoc$m01 <- car::recode(proc_elsoc$m01, "c(1,2)=1; c(3)=2;c(4,5)=3;c(6,7)=4;c(8,9,10)=5")
proc_elsoc$m0_sexo <- car::recode(proc_elsoc$m0_sexo, "1=0;2=1")

## Etiquetado
# Meritocracia= esfuerzo y talento
proc_elsoc <- proc_elsoc %>% rename("mesfuerzo"=c18_09, # meritocracia esfuerzo
                                    "mtalento" =c18_10) # meritocracia talento

get_label(proc_elsoc$mesfuerzo) # Visualizar
proc_elsoc$mesfuerzo <- set_label(x = proc_elsoc$mesfuerzo,label = "Recompensa: esfuerzo") # Renombrar
get_label(proc_elsoc$mtalento) # Visualizar
proc_elsoc$mtalento  <- set_label(x = proc_elsoc$mtalento, label = "Recompensa: talento") # Renombrar

proc_elsoc$pmerit <- (proc_elsoc$mesfuerzo+proc_elsoc$mtalento)/2 # Creacion de la nueva variable llamada "pmerit"
summary(proc_elsoc$pmerit)
get_label(proc_elsoc$pmerit) # Visualizar su etiqueta. Vemos que todavia tiene la etiqueta de la variable "Recompensa: esfuerzo".
proc_elsoc$pmerit  <- set_label(x = proc_elsoc$pmerit, label = "Meritocracia promedio") # Cambio etiqueta

# Educacion
proc_elsoc$m01 <- set_labels(proc_elsoc$m01,
                             labels=c( "Primaria incompleta menos"=1,
                                       "Primaria y secundaria baja"=2,
                                       "Secundaria alta"=3,
                                       "Terciaria ciclo corto"=4,
                                       "Terciaria y Postgrado"=5))

proc_elsoc <- rename(proc_elsoc,"edcine"=m01)
get_label(proc_elsoc$edcine) #Visualizar
proc_elsoc$edcine <- set_label(x = proc_elsoc$edcine,label = "Educacion")

# Estatus social subjetivo
proc_elsoc <- proc_elsoc %>% rename("ess"=d01_01)
get_label(proc_elsoc$ess) # Visualizar
proc_elsoc$ess <- set_label(x = proc_elsoc$ess,label = "Estatus Social Subjetivo") # Cambiar

# Sexo
proc_elsoc$m0_sexo <- set_labels(proc_elsoc$m0_sexo,
                                 labels=c( "Hombre"=0,
                                           "Mujer"=1)) # Cambio de etiquetas
proc_elsoc <- rename(proc_elsoc,"sexo"=m0_sexo) # Cambiar nombre
get_label(proc_elsoc$sexo) # Visualizar
proc_elsoc$sexo <- set_label(x = proc_elsoc$sexo,label = "Sexo") # Cambiar

# Edad
proc_elsoc <- rename(proc_elsoc,"edad"=m0_edad)
get_label(proc_elsoc$edad) # Visualizar
proc_elsoc$edad <- set_label(x = proc_elsoc$edad,label = "Edad") # Cambiar

## Revisamos
frq(proc_elsoc$mesfuerzo)
frq(proc_elsoc$mtalento)
frq(proc_elsoc$pmerit)
frq(proc_elsoc$m01)
frq(proc_elsoc$sexo)

# 5. Exportar base procesada ---- 
stargazer(proc_elsoc, type="text")
#save(proc_elsoc,file = "[ruta hacia carpeta local en su computador]/ELSOC_ess_merit2016.RData")
save(proc_elsoc,file = "output/ELSOC_ess_merit2016.RData")