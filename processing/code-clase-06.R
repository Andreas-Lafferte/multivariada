# ---- 0. Identificacion y descripción general ---- 

---
  # Título: "Práctica 6"
  # Autores: "Grupo 5 y Grupo 7 EMV 2020"
  # Fecha: "2 de julio de 2020"
---
  
## El objetivo de esta practica es profundizar en el significado de la parcializacion en la regresion multiple. 
## Para ello utilizaremos el ejemplo 3.1 de Wooldridge (2010) cap. 3 (Análisis de regresion múltiple) (p.68-80) sobre las determinantes del promedio en la universidad.
  
# ---- 1. Librerias ----

## Fijamos directorio de trabajo
## Librerias 

pacman::p_load(ggpubr, #graficos
               dplyr, #manipulacion de datos
               sjPlot, #tablas
               gridExtra, #unir graficos
               texreg, #mostrar regresion multiple
               summarytools, #estadisticos descriptivos
               wooldridge) #paquete con los ejemplos del libro

library(wooldridge)

# ---- 2. Datos ----
## Los datos a utilizar corresponden a la base de datos gpa1 
## que incluye una muestra de 141 estudiantes de una universidad. La base contiene variables: 

# [colGPA]: promedio general de calificaciones de la universidad, en escala de 0 a 4 puntos. 
# [hsGPA]: promedio general de calificaciones en la enseñanza media, en escala de 0 a 4 puntos 
# [ACT]: puntaje en el examen de admisión a la universidad, que va de 16 a 33 punto

data('gpa1') # Cargar base de datos
gpa1 <- dplyr::select(gpa1, colGPA, hsGPA, ACT) #Seleccion de variables

# ---- 3. Descriptivos y relacion entre variables ----

view(dfSummary(gpa1, headings = FALSE, method = "render"))

## Veamos la relacion (distribucion y pendiente) entre las variables dependientes y la dependiente. Ademas, veremos la relacion entre ambas independientes

#Grafico x1 = ACT y= colGPA
gact <- ggscatter(gpa1, x = "ACT", y = "colGPA",
                  shape = 21, size = 3, # Forma y tamaño de puntos
                  add = "reg.line", #Agregar recta de regresion
                  cor.coef = TRUE)# Agregar coeficiente correlacion

#Grafico x2 = hsGPA y= colGPA
ghsGPA <- ggscatter(gpa1, x = "hsGPA", y = "colGPA",
                    shape = 21, size = 3,
                    add = "reg.line",
                    cor.coef = TRUE)

#Grafico x2 = hsGPA x1 = ACT
gact_hs <- ggscatter(gpa1, x = "hsGPA", y = "ACT",
                     shape = 21, size = 3,
                     add = "reg.line",
                     cor.coef = TRUE)

grid.arrange(gact, ghsGPA, gact_hs, nrow = 1) # Unir graficos

## Con este grafico podemos notar que si bien ambas variables tienen una asociacion positiva con colGPA
## su tamaño de efecto es distinto. De hecho, la nueva variable introducida hsGPA tiene una asoaicion mas grande con nuetsra variable dependiente. 

## Ahora bien, la pregunta que nos haciamos en la practica pasada era: ¿como inciden ACT y hsGPA conjuntamente sobre colGPA?
## Es decir; ¿como inciden nuestras variables independiente X1,X2,...Xn sobre nuestra variable dependiente Y?

## Pero no nos preguntamos por cómo se relacionan nuestras variables independientes sobre nuestra variable dependiente. 
## Como podemos ver, existe una relación entre las calificaciones en la enseñanza media (X1) y el puntaje en la prueba de admisión (X2). 
## Específicamente, ambas variables tienen una asociación positiva de 0.35

## ¿Qué implica que nuestros predictores estén correlacionados? 

# ---- 4. Modelo de regresion multiple ---- 

## Retomemos el modelo de la practica anterior: 

col_actmodel<-lm(colGPA ~ ACT, data=gpa1)
col_hsmodel<-lm(colGPA ~  hsGPA, data=gpa1)
col_model <- lm(colGPA ~ ACT + hsGPA, data = gpa1)

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model), show.ci=FALSE, p.style = "asterisk", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),string.pred = "Predictores", string.est = "??")

# ---- 5. Interpretación ---- 
# Pueden visitar el siguiente link: https://spark.adobe.com/page/NIkrQh58TJtwj/


## Regresiones simples: 
# Modelo 1: por cada punto que aumenta el examen de admision [ACT], el promedio universitario [colGPA] aumentara en 0.03 ptos.

# Modelo 2: por cada punto que aumenta las notas en enseñanza media [hsGPA], el promedio universitario [colGPA] aumentara en 0.48 ptos.

## Regresion multiple:
## Modelo 3: 

# Coeficiente de regresión de hsGPA: cada punto más en hsGPA se relaciona con un aumento en 0.453 puntos adicionales en colGPA (casi medio punto), manteniendo ACT constante o ceteris paribus.
# Coeficiente de regresión de ACT: cada aumento en un punto en ACT se relaciona con un aumento en 0.01 puntos adicionales en colGPA, manteniendo hsGPA constante o ceteris paribus.


## Podemos notar que al incorporar mas variables al modelo se descuente este elemento comun que tienen las variables independientes.
## Así, tanto los coeficientes de regresion de ambos predictores se parcializan y tambien la R2 ajustada.

# ---- 6. Parcializacion ---- 
## ¿Cómo vamos con la parcializacion? Si hay dudas revisar el siguiente link de explicacion: https://spark.adobe.com/page/3qKpLyG2SnZCO/

## Para parcializar seguimos el siguiente flujo: 
# 1) regresion entre predictores;
# 2) obtencion del residuo de su regresion y 
# 3) regresion de Y en el residuo (variable parcializada)


# Paso 1: Estimar modelo 

model_act_hs = lm(ACT ~ hsGPA, data = gpa1) #Crear regresion con predictores. Notese que estamos parcializando ACT (X1)
coef(model_act_hs)

# Paso 2: Calcular valores predichos 

fit_act_hs=fitted.values(model_act_hs) # Calcular valores predichos
res_act_hs=residuals(model_act_hs) #Calcular residuos
gpa1=cbind(gpa1, fit_act_hs,res_act_hs) # Unir columna de residuos y valores predichos a base de datos
head(gpa1) #Mostrar los primeros elementos de la base de datos

# Paso 3: Crear regresión con variable parcializada 

act_hs_model <- lm(colGPA ~ res_act_hs, data = gpa1) # Estimar regresión simple con parcialización de ACT

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model, act_hs_model), show.ci=FALSE, p.style = "asterisk", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores", string.est = "??")

# ---- 7. Control estadístico ---- 

## Veamos el impacto que tiene cada predictor sobre colGPA para interpretar:

plot_model(col_model, show.values = TRUE)+ theme_sjplot()

## El efecto de hsGPA sobre colGPA controlando por ACT es mucho mayor que ACT parcializado. 
## Pero para interpretar tenemos que rastrear nuestras hipotesis.

## La regresion multiple imita la situacion constante de una variable sin restringir los valores de ningun predictor. 

