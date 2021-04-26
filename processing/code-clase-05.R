# Codigo practica 5: Regresion multiple.

# 1. Cargar librerias ----
pacman::p_load(ggpubr, #graficos
               dplyr, #manipulacion de datos
               sjPlot, #tablas
               gridExtra, #unir graficos
               texreg, #mostrar regresion multiple
               summarytools, #estadisticos descriptivos
               wooldridge) #paquete con los ejemplos del libro

# 2. Cargar datos ----

# [colGPA]: promedio general de calificaciones de la universidad, escala de 0 a 4 puntos 
# [hsGPA]: promedio general de calificaciones en la ense?anza media, escala de 0 a 4 puntos 
# [ACT]: puntaje en el examen de admision a la universidad, que va de 16 a 33 puntos

data('gpa1') 

gpa1 <- dplyr::select(gpa1, colGPA, hsGPA, ACT) 

# 3. Descriptivos ----

view(dfSummary(gpa1, headings = FALSE, method = 'render'))

# 4. Modelo regresion simple ---- 

col_actmodel<-lm(colGPA ~ ACT, data=gpa1) 

summary(col_actmodel)

sjPlot::tab_model(col_actmodel, show.ci=FALSE) 

# 5. Modelo regresion multiple----

## Graficamos asociacion con variables ACT y hsGPA 

#Grafico x1 = ACT
gact <- ggscatter(gpa1, x = "ACT", y = "colGPA",
                  shape = 21, size = 3, # Forma y tama?o de puntos
                  add = "reg.line", #Agregar recta de regresion
                  cor.coef = TRUE)# Agregar coeficiente correlacion

#Grafico x2 = hsGPA
ghsGPA <- ggscatter(gpa1, x = "hsGPA", y = "colGPA",
                    shape = 21, size = 3,
                    add = "reg.line",
                    cor.coef = TRUE)

grid.arrange(gact, ghsGPA, nrow = 1) # Unir graficos

## ¿Como inciden ACT y hsGPA conjuntamente sobre colGPA?

## 5.1. Modelo ----

col_actmodel<-lm(colGPA ~ ACT, data=gpa1)
col_hsmodel<-lm(colGPA ~  hsGPA, data=gpa1)
col_model <- lm(colGPA ~ ACT + hsGPA, data = gpa1)

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),string.pred = "Predictores", string.est = "β")

# 7. Interpretacion ----

#Ver sitio web o el siguiente enlace: https://spark.adobe.com/page/NIkrQh58TJtwj/ 