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

sjPlot::tab_model(list(m00, m01, m02, m03), show.ci=FALSE, p.style = "stars", dv.labels = c("Modelo 0", "Modelo 1", "Modelo 2", "Modelo 3"),string.pred = "Predictores", string.est = "β")

