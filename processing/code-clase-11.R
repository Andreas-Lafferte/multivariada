# Codigo practica 11: Transformacion de variables y supuestos.

# 1. Cargar librerias ----
pacman::p_load(dplyr, 
               summarytools, 
               sjPlot,
               texreg, 
               corrplot,
               ggplot2,
               ggfortify,
               sandwich,
               lmtest,
               sjlabelled)

# 2. Cargar datos ---- 
load(url("https://multivariada.netlify.com/assignment/data/elsoc18p11.RData"))

# 3. Descriptivos ----

view(dfSummary(elsoc, headings = FALSE, method = "render"))

view_df(elsoc,max.len = 50)

# 4. Medicion y transformacion de variables ---- 

# 4.1. Crear indice PP ---- 

## 4 preguntas referentes a participacion politica (likert)

plot_stackfrq(elsoc[,c("part01","part02","part03","part04")]) + theme(legend.position="bottom")

## Para crear un indice primero debemos verificar si nuestros indicadores estan correlacionados

corrplot.mixed(cor(select(elsoc,part01,part02,part03,part04),
                   use = "complete.obs"))

## Crear un indice sumatorio simple

elsoc <- elsoc %>% mutate(partpol=rowSums(select(., part01,part02,part03,part04))) 

descr(elsoc$partpol,style = "rmarkdown",stats = "common", transpose = T,headings = F)

plot_frq(data = elsoc$partpol,type = "hist",show.mean = T)

# 4.2. Missing's values ---- 

## Veamos el caso de los ingresos que es clasico. 
# ELSOC tiene dos variables para tratar de palear estas complejidades [inghogar] y [inghogar_t] 

descr(elsoc$inghogar,style = "rmarkdown",stats = "common", transpose = T,headings = F)

sjmisc::frq(elsoc$inghogar_t,
            out = "txt",
            show.na = T) %>% knitr::kable()

# Dado que en [inghogar] hay un 17.82% de NA's, se usaran los datos disponibles en [inghogar_t]
## Pasos: 
# 1) Calcular la media del tramo reportado 
# 2) En caso de que un caso no reporte el monto ingreso hogar, reemplazaremos este valor peridodo con la media de [inghogar_t]
# 3) Comparamos la variable original con la nueva que posee informacion 


# -- Paso 1 --
elsoc$inghogar_t[elsoc$inghogar_t==1] <-(       220000 )    # [1]  "Menos de $220.000 mensuales liquidos"          
elsoc$inghogar_t[elsoc$inghogar_t==2] <-(220001 +280000 )/2 # [2]  "De $220.001 a $280.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==3] <-(280001 +330000 )/2 # [3]  "De $280.001 a $330.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==4] <-(330001 +380000 )/2 # [4]  "De $330.001 a $380.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==5] <-(380001 +420000 )/2 # [5]  "De $380.001 a $420.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==6] <-(420001 +470000 )/2 # [6]  "De $420.001 a $470.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==7] <-(470001 +510000 )/2 # [7]  "De $470.001 a $510.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==8] <-(510001 +560000 )/2 # [8]  "De $510.001 a $560.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==9] <-(560001 +610000 )/2 # [9]  "De $560.001 a $610.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==10]<-(610001 +670000 )/2 # [10] "De $610.001 a $670.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==11]<-(670001 +730000 )/2 # [11] "De $670.001 a $730.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==12]<-(730001 +800000 )/2 # [12] "De $730.001 a $800.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==13]<-(800001 +890000 )/2 # [13] "De $800.001 a $890.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==14]<-(890001 +980000 )/2 # [14] "De $890.001 a $980.000 mensuales liquidos"       
elsoc$inghogar_t[elsoc$inghogar_t==15]<-(980001 +1100000)/2 # [15] "De $980.001 a $1.100.000 mensuales liquidos"      
elsoc$inghogar_t[elsoc$inghogar_t==16]<-(1100001+1260000)/2 # [16] "De $1.100.001 a $1.260.000 mensuales liquidos"    
elsoc$inghogar_t[elsoc$inghogar_t==17]<-(1260001+1490000)/2 # [17] "De $1.260.001 a $1.490.000 mensuales liquidos"    
elsoc$inghogar_t[elsoc$inghogar_t==18]<-(1490001+1850000)/2 # [18] "De $1.490.001 a $1.850.000 mensuales liquidos"    
elsoc$inghogar_t[elsoc$inghogar_t==19]<-(1850001+2700000)/2 # [19] "De $1.850.001 a $2.700.000 mensuales liquidos"    
elsoc$inghogar_t[elsoc$inghogar_t==20]<-(2700000)           # [20] "Mas de $2.700.000 a mensuales liquidos"

# -- Paso 2 --
elsoc$inghogar_i <- ifelse(test = (is.na(elsoc$inghogar)), #¿existen NA en ingresos?
                           yes = elsoc$inghogar_t,         #VERDADERO, remplazar con la media del tramo
                           no = elsoc$inghogar)            #FALSE, mantener la variable original.

elsoc$inghogar_i <- set_label(elsoc$inghogar_i,"Ingreso total del hogar (imputada)")

# -- Paso 3 --
sjmisc::descr(elsoc[,c("inghogar","inghogar_i")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)

## Vemos que pasamos de 17.8% NA's a un 5.1%, es decir, recuperamos 12.72% de casos
## Ahora podemos calcular el ingreso per capita del hogar empleando la variable "cantidad de habitantes hogar"

elsoc$ing_pcap <- elsoc$inghogar_i/elsoc$tamhogar
elsoc$ing_pcap <- set_label(elsoc$ing_pcap,"Ingreso per cápita del hogar")

sjmisc::descr(elsoc[,c("inghogar","inghogar_i","tamhogar","ing_pcap")],
              show =c("label", "n", "NA.prc", "mean", "md","sd")) %>% knitr::kable(digits = 2)

# 4.3. Ingresos categorica ---- 

## Teniendo el ingreso per capita podemos calcular categorias de ingresos como quintiles o deciles. 

elsoc$quintile<- dplyr::ntile(x = elsoc$ing_pcap,
                              n = 5) # n de categorias, para quintiles usamos 5 

elsoc$quintile <- factor(elsoc$quintile,c(1,2,3,4,5), c("Quintil 1","Quintil 2","Quintil 3","Quintil 4","Quintil 5")) 
elsoc %>% 
  group_by(quintile) %>% 
  summarise(n=n(),
            Media=mean(ing_pcap,na.rm = T),
            Mediana=median(ing_pcap,na.rm = T)) %>% 
  knitr::kable()

## Si queremos recuperar ese 5.23% (n=196) de NA's podemos generar una categoria adicional
## para los datos perdidos, vale decir, recodificamos los NA's para que se incluyan como nueva categoria

elsoc$quintilemiss <- factor(elsoc$quintile,ordered = T)
elsoc$quintilemiss <- ifelse(is.na(elsoc$quintilemiss),yes = 6,no = elsoc$quintilemiss)
elsoc$quintilemiss <- factor(elsoc$quintilemiss ,levels = c(1,2,3,4,5,6),labels =  c("Quintil 1","Quintil 2","Quintil 3","Quintil 4","Quintil 5","Missing")) 
elsoc %>% group_by(quintilemiss) %>% summarise(n=n())

# 5. Modelos ----

fit01<- lm(partpol~sexo+edad+ing_pcap+pospol,data=elsoc)
fit02<- lm(partpol~sexo+edad+quintile+pospol,data=elsoc)
fit03<- lm(partpol~sexo+edad+quintilemiss+pospol,data=elsoc)

labs01 <- c("Intercepto","Sexo (mujer=1)","Edad","Ingreso per/cap","Centro (ref. derecha)","Izquierda","Idep./Ninguno",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5","Quintil perdido")

screenreg(list(fit01,fit02,fit03),doctype = FALSE, 
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3"),
        custom.coef.names = labs01)

# 6. Diagnostico supuestos ---- 

# 6.1. Casos influyentes ---- 

## Para determinar casos influyentes usamos D de Cook. Luego se establece el punto de corte 4/(n-k-1) y se compara

n<- nobs(fit03) #n de observaciones
k<- length(coef(fit03)) # n de parametros
dcook<- 4/(n-k-1) #punt de corte

# Graficamos 
final <- broom::augment_columns(fit03,data = elsoc)
final$id <- as.numeric(row.names(final))
# identify obs with Cook's D above cutoff
ggplot(final, aes(id, .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dcook)+
  geom_text(aes(label=ifelse((.cooksd>dcook),id,"")),
            vjust=-0.2, hjust=0.5)

# identidicamos casos influyentes y filtramos la base de datos
ident<- final %>% filter(.cooksd>dcook)
elsoc02 <- final %>% filter(!(id %in% ident$id))

# estimacion sin casos influyentes 
fit04<- lm(partpol~sexo+edad+quintilemiss+pospol,data=elsoc02)

labs02 <- c("Intercepto","Sexo (mujer=1)","Edad",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5","Quintil perdido",
            "Izquierda (ref. derecha)","Centro","Idep./Ninguno")

screenreg(list(fit03,fit04), 
        doctype = FALSE,
        custom.model.names = c("Modelo 3", "Modelo 4"),
        custom.coef.names = labs02)

#  6.2. Linealidad ---- 

## Analisis de la distribucion de los residuos respecto a la recta de regresion 
## Los residuos deben ser independientes de los valores predichos y la presencia de un patron no lineal es señal de que el modelo esta especificando incorrectamente

ggplot(fit04, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = TRUE)

## Para mejorar la estimacion haremos una transofrmacion de variables: edad e ingresos

## Polinomio de Edad = Edad^2
elsoc02$edad2 <- elsoc02$edad^2
fit05<- lm(partpol~sexo+edad+edad2+quintilemiss+pospol,data=elsoc02)

edad<- fit05$model$edad
fit<- fit05$fitted.values
data01 <- as.data.frame(cbind(edad,fit))

ggplot(data01, aes(x = edad, y = fit)) +
  theme_bw() +
  geom_point()+
  geom_smooth()

## Logaritmo de ingreso = log(ingreso)
elsoc02$lningreso <- log(elsoc02$ing_pcap)
elsoc02$lningreso <- set_label(elsoc02$lningreso,"log(ingreso per cap)")
fit06 <- lm(partpol~sexo+edad+edad2+lningreso+pospol,data=elsoc02)

plot_frq(elsoc02$ing_pcap,type = "hist",normal.curve = T, show.mean = T)
plot_frq(elsoc02$lningreso,type = "hist", normal.curve = T,show.mean = T)

## Comparamos modelos 

labs03 <- c("Intercepto","Sexo (mujer=1)","Edad",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5","Quintil perdido",
            "Izquierda (ref. derecha)","Centro","Idep./Ninguno", "Edad²","Ingreso per cap (log)")

screenreg(list(fit04, fit05, fit06), doctype = FALSE,
        custom.model.names = c("Modelo 4", "Modelo 5", "Modelo 6"), 
        custom.coef.names = labs03)

## Interpretacion: 
# El beta de log(ingreso)= 0.24 
# Por una unidad de porcentaje (1%) que se incrementen los ingresos per capita del hogar, el promedio del indice de participacion politica aumenta en 0.24 (o bien un 0.0024) ceteris paribus.

# 6.3. Homocedasticidad ---- 
car::ncvTest(fit05)
lmtest::bptest(fit05)

## Tanto el test de Breush-Pagan como el de Cook-Weisberg indican que hay problemas de heterocedasticidad de los residuos

## Calcular los errores robustos para corregir los problemas de heterocedasticidad 
model_robust<- coeftest(fit05, vcov=vcovHC)

# Comparamos modelos 

labs04 <- c("Intercepto","Sexo (mujer=1)","Edad",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5","Quintil perdido",
            "Izquierda (ref. derecha)","Centro","Idep./Ninguno", "Edad²")

screenreg(list(fit04, fit05, model_robust), doctype = FALSE,
        custom.model.names = c("Modelo 4","Modelo 5", "M5 Robust"), custom.coef.names = labs04)

## Con ES robusto indican que las estimaciones son robustas a la presencia de heterocedasticidad en los residuos, 
## debido a la mantencion de la significancia estadistica. En suma, la heterocedasticidad no es problematica

# 6.4. Multicolinealidad ---- 
car::vif(fit04)
car::vif(fit05)

## Vemos que en el modelo que no incorpora edad cuadratica no hay problemas de multicolinealidad, en los que sí es problematico.