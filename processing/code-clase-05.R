# ---- 0. Identificacion y descripción general ---- 

---
  # Título: "Práctica 5"
  # Autores: "Ivankovic, Jaime, Lara, Leiva, Lovazzano, Martínez & Miranda"
  # Fecha: "25 de junio de 2020"
---
  
## El objetivo de esta practica es introducirnos a la regresion multiple. Una vez comprendida la regresion simple podemos
## podemos situarnos a comprender los fenomenos sociales como multicausales. Para ello utilizaremos el ejemplo 3.1 de Wooldridge 
## (2010) cap. 3 (Análisis de regresion múltiple) (p.68-80) sobre las determinantes del promedio en la universidad.
  
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

# ---- 2. Datos ----

## Los datos a utilizar corresponden a la base de datos gpa1 
## que incluye una muestra de 141 estudiantes de una universidad. La base contiene variables: 

# [colGPA]: promedio general de calificaciones de la universidad, en escala de 0 a 4 puntos. 
# [hsGPA]: promedio general de calificaciones en la enseñanza media, en escala de 0 a 4 puntos 
# [ACT]: puntaje en el examen de admisión a la universidad, que va de 16 a 33 punto

data('gpa1') # Cargar base de datos
gpa1 <- dplyr::select(gpa1, colGPA, hsGPA, ACT) #Seleccion de variables

# ---- 3. Descriptivos ----

view(dfSummary(gpa1, headings = FALSE, method = 'render'))

# ---- 4. Regresion Simple ---- 

## Si nos basamos solo en un modelo de regresion simple, lo mas logico seria predecir 
## las calificaciones de la universidad a partir del puntaje obtenido en la prueba de admisión a esta. 

col_actmodel<-lm(colGPA ~ ACT, data=gpa1) #Crear regresion simple
summary(col_actmodel)
sjPlot::tab_model(col_actmodel, show.ci=FALSE) #Tabla resumen de resultados

## Esto nos dice: por cada punto en el examen de admision a la universidad, el promedio de calificaciones universitario 
## aumenta en 0.03 puntos promedio. 

## Si miramos nuestra R2 podemos notar que ACT (nuestra variable independiente) solo explica un 4.3% de la varianza de colGPA (dependiente). 
## Por ello, para mejorar nuestro modelo y acercanos a predicciones mas plausibles (multicausales) introduciremos
## la variable [hsGPA] que es el promedio general de calificaciones en la enseñanza media.  

# ---- 5. Comparacion entre variables ----

## Graficaremos las variables que determinarian colGPA para comparar sus distribuciones y pendientes (b) de sus respectivas regresiones simples. 

#Grafico x1 = ACT
gact <- ggscatter(gpa1, x = "ACT", y = "colGPA",
                  shape = 21, size = 3, # Forma y tamaño de puntos
                  add = "reg.line", #Agregar recta de regresion
                  cor.coef = TRUE)# Agregar coeficiente correlacion
#Grafico x2 = hsGPA
ghsGPA <- ggscatter(gpa1, x = "hsGPA", y = "colGPA",
                    shape = 21, size = 3,
                    add = "reg.line",
                    cor.coef = TRUE)

grid.arrange(gact, ghsGPA, nrow = 1) # Unir graficos


## Con este grafico podemos notar que si bien ambas variables tienen una asociacion positiva con colGPA
## su tamaño de efecto es distinto. De hecho, la nueva variable introducida hdGPA tiene una asoaicion mas grande con nuetsra variable dependiente. 

## Ahora bien, la pregunta de regresion multiple es: ¿como inciden ACT y hsGPA conjuntamente sobre colGPA?
## Es decir; ¿como inciden nuestras variables independiente X1,X2,...Xn sobre nuestra variable dependiente Y?

# ---- 6. Regresion multiple ---- 

## Para estimar el modelo de regresion multiple se debe realizar el mismo procedimiento de la regresion simple, solo que ahora deben señalar un (+) y el segundo predictor

col_actmodel<-lm(colGPA ~ ACT, data=gpa1)
col_hsmodel<-lm(colGPA ~  hsGPA, data=gpa1)
col_model <- lm(colGPA ~ ACT + hsGPA, data = gpa1)

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model), show.ci=FALSE, p.style = "asterisk", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),string.pred = "Predictores", string.est = "??")

# ---- 7. Interpretación ---- 

#Ver siguiente enlace: https://spark.adobe.com/page/NIkrQh58TJtwj/ 