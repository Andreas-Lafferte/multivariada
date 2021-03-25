# Codigo practica 2: Descripcion de variables en R.

# 1. Cargar librerias ----
pacman::p_load(dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y graficos
               corrplot, # Correlaciones
               sessioninfo) # Información de la sesion de trabajo

# 2. Cargar datos ----
## Desde internet
load(url("https://multivariada.netlify.app/assignment/data/proc/ELSOC_ess_merit2016.RData"))

names(proc_elsoc)
dim(proc_elsoc) 
sapply(proc_elsoc, class) #naturaleza variables de la base

# 3. Descriptivos ----
## Tablas descruptivas

# Tabla stargazer
stargazer(proc_elsoc,type = "text") 

# Tabla descr (librería sjmisc)
sjmisc::descr(proc_elsoc)

sjmisc::descr(proc_elsoc,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown") 

#Tabla summarytools::dfSummary
dfSummary(proc_elsoc, plain.ascii = FALSE)

view(dfSummary(proc_elsoc, headings=FALSE))

# 3.1. Extraer casos perdidos (NA) ----
proc_elsoc_original <- proc_elsoc # Respaldo base original
dim(proc_elsoc) 

sum(is.na(proc_elsoc)) # Cantidad NA en la base

proc_elsoc <-na.omit(proc_elsoc) # Eliminar NA
dim(proc_elsoc) 

proc_elsoc <- sjlabelled::copy_labels(proc_elsoc,proc_elsoc_original) # Restaurar etiquetas de base respaldada

# 4. Bivariados ----
## Tabla de contingencia para variables categoricas
sjt.xtab(proc_elsoc$edcine, proc_elsoc$sexo) #Tabla de contingencia


sjt.xtab(proc_elsoc$edcine, proc_elsoc$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE)

## Tabla de estadisticos de variable continua por categorias
tapply(proc_elsoc$pmerit, proc_elsoc$edcine, mean)

proc_elsoc %>% # se especifica la base de datos
  select(pmerit,edcine) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(edcine)) %>% # se agrupan por la variable categorica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(pmerit),SD=sd(pmerit)) %>% # se agregan las operaciones a presentar en la tabla
  kable(, format = "markdown") # se genera la tabla

## Graficos de caja y bigote por categoría
plot_grpfrq(proc_elsoc$pmerit,proc_elsoc$edcine,
            type = "box")

## Correlaciones 
M <- cor(proc_elsoc) # Basica de visualizar correlaciones
M

## Tablas
tab_corr(proc_elsoc) 

tab_corr(proc_elsoc,
         triangle = "lower") 

## Matriz
corrplot.mixed(M) 

## Grafico (scatter)
plot_scatter(proc_elsoc, edad, ess)

# 5. Nota final ----
sessionInfo() # Informacion sobre la sesion de R
session_info("sessioninfo")$platform #Informacion mas precisa
package_info(pkgs = (.packages()), dependencies = FALSE) # Informacion sobre paquetes