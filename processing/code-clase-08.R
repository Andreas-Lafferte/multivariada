# Codigo practica 8: Tabla de regresion multiple. 

# 1. Cargar librerias ----
pacman::p_load(dplyr,readxl, summarytools, stargazer, equatiomatic)

# 2. Cargar datos ----
data <-read_excel("https://multivariada.netlify.app/assignment/data/hsb2.xls")

# 3. Procesamiento ---- 
names(data)
data <- data %>% select (science,math,female, socst, read)
data <- data %>% rename(ciencia=science, matematicas =math, mujer=female, status=socst, lectura=read)

# 4. Descriptivos ----
print(dfSummary(data, headings = FALSE), method = "render")

# 5. Modelos ----

## La forma de presentar los modelos depende de nuestras hipotesis y de la definicion de los predictores
## principales y los de control

reg1 <- lm(ciencia ~ matematicas + lectura, data=data)
reg2 <- lm(ciencia ~ matematicas + lectura + mujer + status, data=data)

sjPlot::tab_model(list(reg1,reg2))

sjPlot::tab_model(list(reg1,reg2),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "asterisk",
                  dv.labels = c("Modelo 1", "Modelo 2"),
                  string.pred = "Predictores",
                  string.est = "β")

# 6. Interpretacion ----
## Si se quiere revisar pagina web: https://version2020--multivariada.netlify.app/assignment/08-code/

sjPlot::plot_model(reg2,ci.lvl = c(0.95), title = "",vline.color = "grey",line.size = 1)

# R2 y R2 Ajustado