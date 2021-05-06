# Codigo practica 10: Regresion logistica.

# 1. Cargar librerias ----
pacman::p_load(dplyr, summarytools, ggmosaic, sjPlot, texreg)

# 2. Cargar datos ----
load(url("https://multivariada.netlify.com/assignment/data/enacoes.RData"))

# 3. Descriptivos ----
view(dfSummary(enacoes, headings = FALSE, method = "render"))

# 4. Bivariados ----
tab_xtab(var.row = enacoes$voto,enacoes$sexo,show.cell.prc = T,show.summary = F)

tab_xtab(var.row = enacoes$voto,enacoes$educ,
         show.cell.prc = T,show.summary = F, encoding = " ")

ggplot(enacoes) +
  geom_mosaic(aes(x=product(voto, sexo), fill=sexo)) +
  geom_label(data = layer_data(last_plot(), 1),
             aes(x = (xmin + xmax)/ 2,
                 y = (ymin + ymax)/ 2,
                 label = paste0(round((.wt/sum(.wt))*100,1),"%"))) +
  labs(y = "Voto última elección",
       x = "Sexo") +
  scale_fill_discrete(name = "",
                      labels = c("Hombre","Mujer"))+
  scale_y_continuous(breaks = c(0,1),
                     labels = c("No votó","Votó")) +
  theme(legend.position="bottom")


ggplot(enacoes) +
  geom_mosaic(aes(x=product(voto, educ), fill=educ)) +
  geom_label(data = layer_data(last_plot(), 1),
             aes(x = (xmin + xmax)/2,
                 y = (ymin + ymax)/2,
                 label = paste0(round((.wt/sum(.wt))*100,1),"%"))) +
  labs(y = "Voto última elección",
       x = "Nivel Educacional") +
  scale_fill_discrete(name = "",
                      labels = c("Primaria incompleta o menos",
                                 "Primaria",
                                 "Secundaria", "Técnica o Superior",
                                 "Universitaria o postgrado"))+
  scale_y_continuous(breaks = c(0,1),labels = c("No votó","Votó")) +
  theme(legend.position="bottom")

# 5. Modelos ----
m00 <- glm(voto~1,data = enacoes,family = "binomial")
m01 <- glm(voto~sexo,data = enacoes,family = "binomial")
m02 <- glm(voto~edad,data = enacoes,family = "binomial")
m03 <- glm(voto~sexo+edad+educ,data = enacoes,family = "binomial")

screenreg(l = list(m00, m01, m02, m03), doctype = FALSE,
          custom.coef.names=c("Intercepto","Sexo (Mujer=1)","Edad","Primaria","Secundaria", "Técnica o Superior","Universitaria o postgrado"),
          custom.model.names = c("Modelo 0","Modelo 1","Modelo 2", "Modelo 3"))

# 5.1. Modelo nulo ----

## El modelo sin predictores nos permite conocer la probabilidad de votar. En este caso, vemos que se 
## obtiene un intercepto de 0.71 correspondiente a [log(p/1-p)]. En este caso es la probabilidad de que 
## una persona participe en las elecciones como votante (voto = 1)

## En otras palabras el intercepto del modelo sin predictores nos entrega la log-odds de votar en las elecciones. 
## Adicionalmente, podemos transformar nuevamente a unidades de probabilidad de la siguiente manera: 

p = exp(0.71)/(1 +  exp(0.71))
p

# 5.2. Modelo con predictor binario ----

## Los resultados muestran el valor del intercepto (0.77) y para sexo (-0.10).
## ¿cuales son las chances (odds) de votar para los hombres y para las mujeres? 

(549/803)/(254/803) # hombres
(776/1171)/(395/1171) # mujeres

(776/395)/(549/254) # odds ratio mujeres sobre hombres
##  Las odds de votar en las elecciones son son 9,5% más bajas para las mujeres respecto a los hombres.

## Usando las odds de votar para los hombres calculada en el apartado anterior, 
## podemos confirmarlo: log(2.16) = 0.77. A su vez, el log-odds para las mujeres corresponde al 
## log del odds-ratio de las odds de las mujeres y de los hombres calculado 
## anteriormente: log(0.907) = -0.10. El output tradicional de R cuando usamos glm() nos entrega los 
## coeficientes en unidades de log-odds, por tanto es necesario realizar la exponenciación de dichos 
## coeficientes para realizar la interpretación en unidades de odds-ratio.

# 5.3. Modelo con predictor continuo ----

## La ecuacion de regresion para la variable edad es: logit(p) = -1.07 + 0.04 * edad. Revisar sito web.

## log odds: por una unidad de incremento en edad (1 año), el cambio esperado en los log-odds es de 0.04.

## odds: por el incremento de una unidad en edad (1 año), se espera ver un incremento aproximado de un 4% en las odds de ir a votar}.

# 5.4. Modelo multivariado ----

## Cada coeficiente prepresenta el cambio predicho en el log-odds de votar por un incremento/cambio en la 
## variable, manteniendo todas las demás variables constantes. Cada coeficiente exponenciado (exp), 
## representa el ratio de dos odds (por ejemplo, mujer respecto a hombre), o el cambio en las odds por el 
## incremento de una unidad de la variable independiente (por ejemplo, un año de edad), manteniendo las demás
## variables constantes.

## Odds ratio mujeres sobre hombres = -0.02 -> las odds de votar en las elecciones son 1,9% mas bajas para las mujeres que los hombres, ceteris paribus.
## Repetir

# 5.4.1. Probabilidades predichas ----
m03$coefficients

# calculamos log-odds
# Intercept     sexo*1    edad*55    educ2       educ3      educ4      educ5
ed01<- -2.03578412+(-0.0235*1)+(0.0491*55)+(0.3365*0)+(0.4691*0)+(0.8844*0)+(1.5260*0) # mujer 55 anios Primaria incompleta
ed02<- -2.03578412+(-0.0235*1)+(0.0491*55)+(0.3365*1)+(0.4691*0)+(0.8844*0)+(1.5260*0) # mujer 55 anios Primaria
ed03<- -2.03578412+(-0.0235*1)+(0.0491*55)+(0.3365*0)+(0.4691*1)+(0.8844*0)+(1.5260*0) # mujer 55 anios Secundaria
ed04<- -2.03578412+(-0.0235*1)+(0.0491*55)+(0.3365*0)+(0.4691*0)+(0.8844*1)+(1.5260*0) # mujer 55 anios Tecnica
ed05<- -2.03578412+(-0.0235*1)+(0.0491*55)+(0.3365*0)+(0.4691*0)+(0.8844*0)+(1.5260*1) # mujer 55 anios Universitaria

# calculamos prob predichas
pr.ed01<- exp(ed01)/(1+exp(ed01))
pr.ed02<- exp(ed02)/(1+exp(ed02))
pr.ed03<- exp(ed03)/(1+exp(ed03))
pr.ed04<- exp(ed04)/(1+exp(ed04))
pr.ed05<- exp(ed05)/(1+exp(ed05))

plot_model(m03,type = "pred",
           terms = "educ",
           title = "Probabilidades predichas para Voto según nivel educacional") + geom_line()

# 6. Medidas de ajuste ----

# 6.1. Devianza ----

## Comparar las verosimilitudes del modelo con predictores, respecto a un modelo con menos predictores
test01<- anova(m00,m01,test = "Chisq")
test02<- anova(m00,m02,test = "Chisq")
test03<- anova(m00,m03,test = "Chisq")
lrt01<- rbind(test01,test02,test03) %>% unique()
row.names(lrt01) <- c("Modelo nulo",
                      "Modelo 1",
                      "Modelo 2",
                      "Modelo 3")
knitr::kable(lrt01,digits = 3, caption = "Test de devianza entre modelos")

# guardar valores de p
test.pvalues1<- test01$`Pr(>Chi)`[2]
test.pvalues2<- test02$`Pr(>Chi)`[2]
test.pvalues3<- test03$`Pr(>Chi)`[2]

# 6.2. Pseudo R2 McFadden ---- 

## Basada en los likelihood, nos entrega una magnitud comparativa entre el modelo con predictores y el modelo nulo
1-(logLik(m01)[1]/ logLik(m00)[1]) # modelo 1 vs modelo nulo
1-(logLik(m02)[1]/ logLik(m00)[1]) # modelo 2 vs modelo nulo
1-(logLik(m03)[1]/ logLik(m00)[1]) # modelo 3 vs modelo nulo

# Con DescTools
mfr2.00 <- DescTools::PseudoR2(m00)
mfr2.01 <- DescTools::PseudoR2(m01)
mfr2.02 <- DescTools::PseudoR2(m02)
mfr2.03 <- DescTools::PseudoR2(m03)

r2<- as.data.frame(cbind(c(mfr2.00,mfr2.01,mfr2.02,mfr2.03)))
rownames(r2) <- c("Modelo nulo",
                  "Modelo 1",
                  "Modelo 2",
                  "Modelo 3")
knitr::kable(r2,digits = 3, col.names = c("McFadden R2"))

# 7. BONUS TRACK: presentacion resultados -----
screenreg(l = list(m03,m03))

or <- texreg::extract(m03)
or@coef <- exp(or@coef)

screenreg(l = list(m03,or), doctype = F,caption = "",caption.above = T,
        custom.model.names = c("Modelo 3", "Modelo 3 (OR)"),
        custom.coef.names=c("Intercepto","Sexo (Mujer=1)","Edad","Primaria","Secundaria", "Técnica o Superior","Universitaria o postgrado"),
        ci.force = c(TRUE,TRUE),
        override.coef = list(coef(m03),or@coef),
        custom.gof.rows=list("Deviance Test ($p$)" = c(test.pvalues3,
                                                       test.pvalues3),
                             "Pseudo R2" = c(mfr2.03,mfr2.03)),
        custom.note = "$^{***}$ p < 0.001; $^{**}$ p < 0.01; $^{*}$ p < 0.05 <br> Errores estándar entre paréntesis. <br> **Nota**: La significancia estadística de los coeficientes en unidades de Odds ratio está calculada en base a los valores $t$, <br> los cuales a su vez se calculan en base a $log(Odds)/SE$")

# graficamente
plot02<- plot_model(m03,vline.color = "grey")
plot01<- plot_model(m03,vline.color = "grey",transform = NULL)
plot_grid(list(plot02,plot01),tags = c(" "," "),
          margin = c(0,0,0,0))