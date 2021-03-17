# ---- 0. Identificacion y descripción general ---- 

---
# Título: "Práctica 7"
# Autores: "Grupo 5 y Grupo 7 EMV 2020"
# Fecha: "9 de julio de 2020"
---
  
## El objetivo de esta practica es abordar los predictores categoricos en regresion y la inferencia estadistica. Ambos temas son independientes entre si pero 
## mediante ellos podemos concatenar la inclusión de predictores categoricos dicotomicos hacia una inferencia estadistica vinculable a la diferencia de promedios en la prueba T.
## Las variables dicotomicas son aquellas variables nominales u ordinales que poseen solo dos categorias de respuesta.
  
## A continuacion veremos un ejemplo a cómo predictores categoricos (dos o más niveles) permiten modelar el Estatus social Subjetivo
  
# ---- 1. Librerias ----

## Fijamos directorio de trabajo
## Librerias 

pacman::p_load(dplyr, #manipulacion de datos
               sjPlot, #tablas
               summarytools, #estadisticos descriptivos
               fastDummies, # Crear variable dummy
               sjlabelled, #etiquetas variables
               ggplot2, # graficos
               coefplot # graficos de coeficientes 
               ) 

# ---- 2. Datos ----

## Los datos a utilizar son de la base ELSOC 2018 con una muestra de 3784 mujeres y hombres aduletos entre 18 y 75 años.
## Variables: 

# [ess]: "Estatus Social Subjetivo: Donde se ubicaria ud. en la sociedad chilena" (0 = el nivel mas bajo; 10 = el nivel mas alto)
# [edcine]: ¿Cuál es su nivel educacional? Indique el tipo de estudio actual (si estudia actualmente) o el último tipo aprobado (si no estudia actualmente) - CINE 2011 (UNESCO).
# [edad]: ¿Cuáles su edad? (años cumplidos).

load(url("https://multivariada.netlify.app/assignment/data/proc/ELSOC_ess.RData")) # Cargar base de datos
view_df(elsoc_18,encoding = "") # examinamos base de datos

# ---- 3. Descriptivos y relacion entre variables ---- 

view(dfSummary(elsoc_18, headings = FALSE, method = "render"))

plot_scatter(data = elsoc_18,x = sexo,y = ess,fit.grps = "lm") 

## Podemos ver que el grafico es poco informativo ya que la variable X (ordenadas) se devide en dos grandes grupos, por lo que los datos de Y (abscisas) se agrupan en torno a ellos. 

## Aplicamos grafico de cajas y bigote para una grafica mas informativa

plot_grpfrq(var.cnt = elsoc_18$ess,var.grp = elsoc_18$sexo,type = "box") # Nos entrega dos medias condicionales (puntos blancos grandes)

## Calculemos el promedio del "ess" exacto en estos dos grupos (hombres y mujeres)
elsoc_18 %>% 
  group_by(sexo) %>% 
  summarise(mean_ess=mean(ess,na.rm = T))

# ---- 4. Regresion ---- 

reg1<-lm(ess ~ sexo, data=elsoc_18)

sjPlot::tab_model(list(reg1), show.ci=FALSE, p.style = "asterisk",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 1"))

## Podemos ver que las mujeres tienen en promedio 0.133 puntos mas que los hombres en la escala de estatus social subjetivo.

## La predeccion o estimacion del ess para sexo es: 

# Para hombres = 4.339
# Para mujeres = 4.472 

## Al cualcular el promedio de ess (Y) para hombres (X0) y mujeres (X1), podemos ver que son los mismos valores que nos entrega la estimacion de la 
## regresion simple donde sexo predice ess. 

## Por tanto, al ingresar un regresor dicotomico en regresion simple lo que se obtiene es una estimacion de la diferencia (resta) de promedios
## de ambas categorias (Y estimado) en relacion a la variable dependiente. 

# ---- 4.1. Regresion multiple ----

# En regresion multiple los coeficientes se parcializan 

reg2<-lm(ess ~ sexo+edad, data=elsoc_18)

sjPlot::tab_model(list(reg1,reg2), show.ci=FALSE, p.style = "asterisk",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# ¿Por que la diferencia promedio de ess entre sexos puede verse afectada por la edad?

elsoc_18 %>%
  group_by(sexo) %>%
  summarise(mean_ess=mean(edad,na.rm = T)) #Revisemos el promedio de edad entre sexos

## Restando ambos promedios, observamos que los hombres tinen un promedio de edad 1,26 mayor que las mujeres. 
## Nuevamente coinciden con el beta de regresion parcializado. Es decir, la diferencia promedio de ess entre sexos se ajusta al controlar por edad. 

# ---- 5. Predictores politomicos ---- 

# Consideramos la educacion en terminos de nivel o grado educacional alcanzado (categorico)

sjmisc::frq(x = elsoc_18$edcine,show.na = F)
plot_frq(data = elsoc_18$edcine) # Grafico distribuciones 

# IMPORTANTE: Para incluir esta variable en al regresion debemos ingresarla como factor!! 

class(elsoc_18$edcine) # naturaleza de la variable
str(elsoc_18$edcine) # forma de los atributos 

# Regresion sin cambiar a factor 

reg3<- lm(ess~edcine,data = elsoc_18)

sjPlot::tab_model(list(reg3), show.ci=FALSE, p.style = "asterisk",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 3"))


# ---- 5.1 Regresion cambiando a factor ---- 
elsoc_18$edcine<- as_factor(elsoc_18$edcine) # convertir a factor

reg4 <- lm(ess~edcine,data = elsoc_18)
sjPlot::tab_model(list(reg3,reg4), show.ci=FALSE, p.style = "asterisk",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 3","Modelo 4"))

## A medida que sube el nivel educacional, sube el promedio de ess. La catgeoria de referencia es primaria imcompleta o menos. 

## Se interpreta de la siguiente manera: el promedio de ess para el grupo con educacion terciaria y posgrado es 1.279 puntos mas alto con respecto
## a las personas con nivel educacional primaria incomplenta. 

## Tambien podemos cambiar la categoria de referencia con  relevel(edcine, ref =X)

reg4.1 <- lm(ess~relevel(edcine,ref=5),data = elsoc_18) # 5 refiere al nivel mas alto en nuestra escala
summary(reg4.1)

# ---- 5.2 Variables Dummy ---- 

## La manera mas sencilla de incluir variables politomicas es por meido de dummy's. 

head(elsoc_18) # observamos filas de la base de datos 
library(fastDummies) # usamos la funcion dummy_cols() del paquete fastDummies
elsoc_18 <- dummy_cols(elsoc_18,select_columns = "edcine") # con select_columns indicamos que variable usaremos 
head(elsoc_18) # volvemos a revisar la base 

## Las dummy's son diferentes a las variable tipo factor. Para agregarlas al modelo debemos hacerlas por considerandolas una a una, y aquella que no se agrege es la de referencia

reg5 <- lm(ess~edcine_2+edcine_3+edcine_4+edcine_5,data = elsoc_18)
sjPlot::tab_model(list(reg4, reg5), show.ci=FALSE, p.style = "asterisk",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 4","Modelo 5"))

## Ambos modelos son identicos solo que en uno las variables son tipo factor y en el segundo dummy. 


# ---- 6. Inferencia estadistica ---- 

## Buscamos la significacion estadistica del coeficiente beta o de regresion (las estrellitas ***)
## Para esta travesia usamos pruebas de hipotesis. Asi, queremos estimar la probabilidad de que Beta sea distino a cero. 

## En regresión nos interesa saber si las diferencias en Y con respecto a los distintos niveles o valores de X son 
## significativas, es decir estadisticamente distintas de 0

# ---- 6.1 Primer paso ----

## creamos una serie de muestras aleatorias a partir de la poblacion. 
## Suponiendo que el promedio estimado para la muestra posee una distribucion normal con un error estandar asociado, podemos calcular
## la probabilidad de error. Recordemos que el promedio +/- 2 SE es el 95% de la distribucion. 

set.seed(123)
elsoc_n30  <- sample_n(tbl = elsoc_18,size = 30 )  %>% mutate(dataset=30 ,mean_ess=mean(ess,na.rm = T))
elsoc_n50  <- sample_n(tbl = elsoc_18,size = 50 )  %>% mutate(dataset=50 ,mean_ess=mean(ess,na.rm = T))
elsoc_n75  <- sample_n(tbl = elsoc_18,size = 75 )  %>% mutate(dataset=75 ,mean_ess=mean(ess,na.rm = T))
elsoc_n100 <- sample_n(tbl = elsoc_18,size = 100)  %>% mutate(dataset=100,mean_ess=mean(ess,na.rm = T))
elsoc_n200 <- sample_n(tbl = elsoc_18,size = 200)  %>% mutate(dataset=200,mean_ess=mean(ess,na.rm = T))
elsoc_n300 <- sample_n(tbl = elsoc_18,size = 300)  %>% mutate(dataset=300 ,mean_ess=mean(ess,na.rm = T))
elsoc_n400 <- sample_n(tbl = elsoc_18,size = 400)  %>% mutate(dataset=400,mean_ess=mean(ess,na.rm = T))
elsoc_n700 <- sample_n(tbl = elsoc_18,size = 700)  %>% mutate(dataset=700,mean_ess=mean(ess,na.rm = T))
elsoc_n800 <- sample_n(tbl = elsoc_18,size = 800)  %>% mutate(dataset=800,mean_ess=mean(ess,na.rm = T))
elsoc_n900 <- sample_n(tbl = elsoc_18,size = 900)  %>% mutate(dataset=900,mean_ess=mean(ess,na.rm = T))
elsoc_n1000<- sample_n(tbl = elsoc_18,size = 1000) %>% mutate(dataset=1000,mean_ess=mean(ess,na.rm = T))
elsoc_n1500<- sample_n(tbl = elsoc_18,size = 1500) %>% mutate(dataset=1500,mean_ess=mean(ess,na.rm = T))
elsoc_n2000<- sample_n(tbl = elsoc_18,size = 2000) %>% mutate(dataset=2000,mean_ess=mean(ess,na.rm = T))
elsoc_n2500<- sample_n(tbl = elsoc_18,size = 2500) %>% mutate(dataset=2500,mean_ess=mean(ess,na.rm = T))
# elsoc      <- elsoc_18 %>% mutate(dataset=3703,mean_ess=mean(ess,na.rm = T))

fullmat<- bind_rows(elsoc_n30 ,elsoc_n50 ,elsoc_n75 ,elsoc_n100,elsoc_n200,elsoc_n300,elsoc_n400,elsoc_n700,elsoc_n800,elsoc_n900,elsoc_n1000,elsoc_n1500,elsoc_n2000,elsoc_n2500)
fullmat <- fullmat %>% mutate(mean_ssta=mean(elsoc_18$ess,na.rm = T)) 

tab_full<- fullmat %>% group_by(dataset) %>% summarise(mean=mean(ess,na.rm = T), sd=sd(ess,na.rm = T),SE=sd/sqrt(n())) # calculamos media, desviacion estandar y error estandar
tab_full

## El ES va disminuyendo a medida que aumenta el tamaño muestral 


# ---- 6.2 Regresion sobre muestras ---- 

# Nos interesa saber si el promedio de Mujeres respecto de Hombres es distinto de 0.

reg100 <- lm(ess~sexo,data=elsoc_n100)
reg1500<- lm(ess~sexo,data=elsoc_n1500)
reg2000<- lm(ess~sexo,data=elsoc_n2000)
reg2500<- lm(ess~sexo,data=elsoc_n2500)

# H0: Si Beta = 0 
# H1: Si Beta distinto de 0

## Los calculos se hacen mediante intervalos de confianza para el coefificnete de regresion tomando un procentaje estandar del 95%. 
## Si ese intervalo no pasa por cero, rechazamos la hipotesis nula. 

# Comprobemos 

# Muestras de distinto tamaño
rbind(broom::tidy(reg100)[2,1:3],  # n=100
      broom::tidy(reg1500)[2,1:3], # n=1500
      broom::tidy(reg2000)[2,1:3], # n=2000
      broom::tidy(reg2500)[2,1:3]) # n=2500

# Calculo intervalos de confianza (IC)

# A. Para 100 casos 
#Beta  +/- 2*SE  = IC
0.0440 - 2*0.314 #intervalo confianza inferior
0.0440 + 2*0.314 #intervalo confianza superior

# B. Para 1500 casos 
0.0489 - 2*0.0829 #intervalo confianza inferior
0.0489 + 2*0.0829 #intervalo confianza superior

# C. Para 2000 casos 
0.145 - 2*0.0715 #intervalo confianza inferior
0.145 + 2*0.0715 #intervalo confianza superior

# D. Para 2500 casos 
0.196 - 2*0.0649 #intervalo confianza inferior
0.196 + 2*0.0649 #intervalo confianza superior

## Podemos ver que para las muestras de 100 y 1500 casos, el IC cruza el cero, por lo que NO rechazamos H0. 
# Esto quiere decir que no hay diferencias estadisticamente signifituvas en el promedio de ess de mujeres respecto a hombres. 

## En las muestras de 2000 y 2500, el IC NO cruza el cero, por lo que rechazamos H0. 
## Esto quiere decir que la diferencia en el promedio de ess de mujeres respecto a hombres es estadisticamente significativa a un 95% de confianza

# Veamoslo visualmente con "coefplot" donde cada punto es el coeficiente de sexo mujer en para cada modelo

coefplot::multiplot(reg2500,reg2000,reg1500,reg100,
                    shorten = T,
                    intercept = F,
                    xlab = "",title = "Modelos según tamaño de muestra",
                    zeroColor = "black",
                    linetype = 1) +
  scale_y_discrete("",labels = c("Sexo (mujer=1)")) +
  theme_bw()



# Resumen
sjPlot::tab_model(list(reg100,reg1500,reg2000,reg2500),
                  dv.labels = c("n=100","n=1500","n=2000","n=2500"),
                  show.se = T,digits = 3,
                  string.est = "??",show.intercept = F,
                  string.ci = "CI 95%",string.se = "SE",
                  show.p = F)