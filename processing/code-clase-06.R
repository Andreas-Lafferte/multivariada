# Codigo practica 6: Regresion multiple.

# 1. Cargar librerias ----
pacman::p_load(ggpubr, #graficos
               dplyr, #manipulacion de datos
               sjPlot, #tablas
               gridExtra, #unir graficos
               texreg, #mostrar regresion multiple
               summarytools, #estadisticos descriptivos
               wooldridge) #paquete con los ejemplos del libro
library(wooldridge)

# 2. Cargar datos ----

# [colGPA]: promedio general de calificaciones de la universidad, escala de 0 a 4 puntos 
# [hsGPA]: promedio general de calificaciones en la ense?anza media, escala de 0 a 4 puntos 
# [ACT]: puntaje en el examen de admision a la universidad, que va de 16 a 33 puntos

data('gpa1') 

gpa1 <- dplyr::select(gpa1, colGPA, hsGPA, ACT) 

# 3. Descriptivos ----
view(dfSummary(gpa1, headings = FALSE, method = "render"))

# 4. Bivariados ----

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

## ¿como inciden nuestras variables independientes X1,X2,...Xn sobre nuestra variable dependiente Y?

## ¿que implica que nuestros predictores esten correlacionados? 

# 4. Modelo de regresion multiple ---- 

col_actmodel<-lm(colGPA ~ ACT, data=gpa1)
col_hsmodel<-lm(colGPA ~  hsGPA, data=gpa1)
col_model <- lm(colGPA ~ ACT + hsGPA, data = gpa1)

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),string.pred = "Predictores", string.est = "β")

# 5. Interpretacion ---- 
# Pueden visitar el siguiente link: https://spark.adobe.com/page/NIkrQh58TJtwj/

## Podemos notar que al incorporar mas variables al modelo se descuenta este elemento comun que tienen las variables independientes.

# 6. Parcializacion ---- 
## Si hay dudas revisar el siguiente link de explicacion: https://spark.adobe.com/page/3qKpLyG2SnZCO/

## Para parcializar seguimos el siguiente flujo: 
# 1) regresion entre predictores;
# 2) obtencion del residuo de su regresion y 
# 3) regresion de Y en el residuo (variable parcializada)


# Paso 1: Estimar modelo 

model_act_hs = lm(ACT ~ hsGPA, data = gpa1) #Crear regresion con predictores
coef(model_act_hs)

# Paso 2: Calcular valores predichos 

fit_act_hs=fitted.values(model_act_hs) # Calcular valores predichos
res_act_hs=residuals(model_act_hs) #Calcular residuos
gpa1=cbind(gpa1, fit_act_hs,res_act_hs) # Unir columna de residuos y valores predichos a base de datos
head(gpa1) #Mostrar los primeros elementos de la base de datos

# Paso 3: Crear regresion con variable parcializada 

act_hs_model <- lm(colGPA ~ res_act_hs, data = gpa1) # Estimar regresión simple con parcialización de ACT

sjPlot::tab_model(list(col_actmodel, col_hsmodel,col_model, act_hs_model), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"),string.pred = "Predictores", string.est = "β")

# 7. Control estadistico ---- 
plot_model(col_model, show.values = TRUE)+ theme_sjplot()

## El efecto de hsGPA sobre colGPA controlando por ACT es mucho mayor que ACT parcializado. 
## Pero para interpretar tenemos que rastrear nuestras hipotesis.

## La regresion multiple imita la situacion constante de una variable sin restringir los valores de ningun predictor. 