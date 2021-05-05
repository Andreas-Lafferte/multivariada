# Codigo practica 7: Inferencia y predictores categoricos.

# 1. Cargar librerias ----
pacman::p_load(dplyr, #manipulacion de datos
               sjPlot, #tablas
               summarytools, #estadisticos descriptivos
               fastDummies, # Crear variable dummy
               sjlabelled, #etiquetas variables
               ggplot2, #graficos
               coefplot) # graficos de coeficientes

# 2. Cargar datos ----

# [ess]: "Estatus Social Subjetivo" (0 = el nivel mas bajo; 10 = el nivel mas alto)
# [edcine]: "Nivel educacional" CINE 2011 (UNESCO)
# [edad]: "Edad"

load(url("https://multivariada.netlify.app/assignment/data/proc/ELSOC_ess.RData")) 

view_df(elsoc_18,encoding = "") 

# 3. Descriptivos ---- 

view(dfSummary(elsoc_18, headings = FALSE, method = "render"))

# 4. Bivariados ----

plot_scatter(data = elsoc_18,x = sexo,y = ess,fit.grps = "lm") 

## Aplicamos grafico de cajas y bigote para una grafica mas informativa

plot_grpfrq(var.cnt = elsoc_18$ess,var.grp = elsoc_18$sexo,type = "box") 

## Calculemos el promedio del "ess" exacto en estos dos grupos (hombres y mujeres)
elsoc_18 %>%
  group_by(sexo) %>%
  summarise(mean_ess=mean(ess,na.rm = T))

# 5. Modelos ---- 

# 5.1. Modelo regresion simple ----

reg1<-lm(ess ~ sexo, data=elsoc_18)

sjPlot::tab_model(list(reg1), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1"))

## Podemos ver que las mujeres tienen en promedio 0.133 puntos mas que los hombres en la escala de ess.

## Por tanto, al ingresar un regresor dicotomico en regresion simple lo que se obtiene es una 
## estimacion de la diferencia (resta) de promedios de ambas categorias (Y estimado) en relacion a la variable dependiente. 

# 5.1. Modelo regresion multiple ----

# En regresion multiple los coeficientes se parcializan 

reg2 <-lm(ess ~ sexo+edad, data=elsoc_18)

sjPlot::tab_model(list(reg1,reg2), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"))

# ¿por que la diferencia promedio de ess entre sexos puede verse afectada por la edad?

elsoc_18 %>%
  group_by(sexo) %>%
  summarise(mean_ess=mean(edad,na.rm = T)) #Revisemos el promedio de edad entre sexos

## Restando, observamos que los hombres tienen un promedio de edad 1,26 mayor que las mujeres. 
## Nuevamente coinciden con el beta de regresion parcializado. Es decir, la diferencia promedio de ess entre sexos se ajusta al controlar por edad. 

# 6. Predictores politomicos ---- 

sjmisc::frq(x = elsoc_18$edcine,show.na = F)
plot_frq(data = elsoc_18$edcine)  

# IMPORTANTE: Para incluir esta variable al modelo debemos ingresarla como factor 

class(elsoc_18$edcine) 
str(elsoc_18$edcine) 

## Regresion sin cambiar a factor 

reg3<- lm(ess~edcine,data = elsoc_18)

sjPlot::tab_model(list(reg3), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 3"))

## Regresion cambiando a factor 

elsoc_18$edcine<- as_factor(elsoc_18$edcine)

reg4 <- lm(ess~edcine,data = elsoc_18)

sjPlot::tab_model(list(reg3,reg4), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 3","Modelo 4"))

# El promedio de ess para el grupo con educacion terciaria y posgrado es 1.279 puntos mas alto con respecto
# a las personas con nivel educacional primaria incomplenta SACAR PARA EL VIDEO.

reg4.1 <- lm(ess~relevel(edcine,ref=5),data = elsoc_18)

summary(reg4.1)

# 7. Variables Dummy ---- 

head(elsoc_18) 
library(fastDummies)
elsoc_18 <- dummy_cols(elsoc_18,select_columns = "edcine") 
head(elsoc_18)

## Para agregar dummy's al modelo debemos ingresarlas una a una y aquella que no es la de referencia

reg5 <- lm(ess~edcine_2+edcine_3+edcine_4+edcine_5,data = elsoc_18)

sjPlot::tab_model(list(reg4, reg5), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "β",digits = 3,
                  dv.labels = c("Modelo 4","Modelo 5"))

# 8. Inferencia estadistica ---- 

## Nos interesa saber si las diferencias en Y con respecto a los distintos niveles o valores de X son 
## significativas, es decir estadisticamente distintas de 0

# 8.1. Procedimiento ----

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

fullmat<- bind_rows(elsoc_n30 ,elsoc_n50 ,elsoc_n75 ,elsoc_n100,elsoc_n200,elsoc_n300,elsoc_n400,elsoc_n700,elsoc_n800,elsoc_n900,elsoc_n1000,elsoc_n1500,elsoc_n2000,elsoc_n2500)
fullmat <- fullmat %>% mutate(mean_ssta=mean(elsoc_18$ess,na.rm = T)) 

tab_full<- fullmat %>% group_by(dataset) %>% summarise(mean=mean(ess,na.rm = T), sd=sd(ess,na.rm = T),SE=sd/sqrt(n()))
tab_full

# 8.2 Regresion sobre muestras ---- 

# Nos interesa saber si el promedio de Mujeres respecto de Hombres es distinto de 0.

reg100 <- lm(ess~sexo,data=elsoc_n100)
reg1500<- lm(ess~sexo,data=elsoc_n1500)
reg2000<- lm(ess~sexo,data=elsoc_n2000)
reg2500<- lm(ess~sexo,data=elsoc_n2500)


# Muestras de distinto tamano
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

# Veamoslo visualmente con "coefplot": cada punto es el coeficiente de sexo mujer 

coefplot::multiplot(reg2500,reg2000,reg1500,reg100,
                    shorten = T,
                    intercept = F,
                    xlab = "",title = "Modelos según tamaño de muestra",
                    zeroColor = "black",
                    linetype = 1) +
  scale_y_discrete("",labels = c("Sexo (mujer=1)")) +
  theme_bw()

sjPlot::tab_model(list(reg100,reg1500,reg2000,reg2500),
                  dv.labels = c("n=100","n=1500","n=2000","n=2500"),
                  show.se = T,digits = 3,
                  string.est = "β",show.intercept = F,
                  string.ci = "CI 95%",string.se = "SE",
                  show.p = F)