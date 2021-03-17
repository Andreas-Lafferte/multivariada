# ---- 0. Identificacion y descripción general ---- 

---
# Título: "Práctica 9"
# Autores: "Grupo 5 y Grupo 7 EMV 2020"
# Fecha: "5 de agosto de 2020"
---
  
## El objetivo de esta practica es introducirnos a los modelos de regresion logistica. La cual 
## es una tecnica de analisis para una regresion en donde nuestra dependiente es dicotomica. Usaremos datos del Titanic


# ---- 1. Librerias ----

## Fijamos directorio de trabajo
## Librerias 

pacman::p_load(dplyr, summarytools, ggmosaic, finalfit)  


# ---- 2. Datos ---- 

# Los datos a usar son un dataset del Titanic. En el hundimiento del Titanic donde murieron 619 personas de las 1046 que iban a bordo. 

#Cargamos la base de datos desde internet
load(url("https://multivariada.netlify.com/assignment/data/titanic.RData"))

# ---- 3. Descriptivos y Bivariados ----
view(dfSummary(tt, headings = FALSE, method = "render")) #exploramos datos

## Nos centraremos en las variables sex y survived. Como podemos notar la categoría de respuesta
## de estas variables son 0 y 1, es decir, son variables dicotómicas.

ctable(tt$survived, tt$sex) # tablas de contingencia 

ggplot(data = tt) +
  geom_mosaic(aes(product(survived, sex), fill= survived)) + labs(y = "Sobreviviente", x = "Sexo") # grafica de mosaico

# ---- 4. Conceptos centrales ----

## Los dos conceptos centrales en regresión logística son las "chances" (o en inglés, odds) y la razón (o en inglés, ratio)

# ---- A. Probabilidades ----

# Una probabilidad es la posibilidad de ocurrencia de un evento o suceso de interés dentro de un conjunto de eventos. 

## Podemos decir que del total de pasajeros, un 40.8% se ellos sobrevive, es decir, la probabildiad de sobrevivir es de 0.408

## Mientras que el 59.2% no sobrevive, por lo que la probabilidad de no sobrevivir es de 0.592

# En R
prop.table(table(tt$survived))

# ---- B. Odds ---- 

# Un Odds se define como la división entre el numero de ocurrencias y el numero de no ocurrencias

# Así, el odds para el ejemplo: 
# Odss= sobrevibientes/nosobrevivientes 

addmargins(table(tt$survived,tt$sex)) # nos entrega las frecuencias marginales y aboslutas para columnas (sexo) y fila (sobreviviencia)

# Calculando los odss obtenemos 0.68 (427/619), es decir, hay 0.68 sobreviviente por cada no sobreviviente. 
# Esto indica que la relacion entre sobrevivientes y no sobrevivientes no es de 1:1, esto indica que existen más chances de morir que sobrevivir ya que es menor a 1=probabilidad de sobrevivir

## Odss de sobreviviencia de hombres y mujeres
# odsshombres = 0.258 (odss menor a 1)
# odssmujeres = 3.04 (odss mayor a 1)

# Odss > 1 indican una chance positiva 
# odss < 1 indican una chance negativa 

prop.table(table(tt$survived,tt$sex),2) # hacemos el caluclo a traves de las probabilidades marginales para cada sexo 


# ---- C. Odds Ratio (OR)

# ¿Cuanto mas sobreviven las mujeres que los hombres? Vale decir, ¿como se expresan las chances en terminos de los betas de regresion?  
# Esto implica la asociacion entre sobreviviena y sexo y ya no solo hablar de las chances de sobreviviencia de cada sexo por separado

# Odss Ratio (razon de chances)

# Odssratio = odssmujeres/odsshombres = 11.78

# INTERPRETACION: LAS CHANCES DE SOBREVIVIR DE LAS MUJERES ES 11.78 VECES MÁS GRANDE QUE LA DE LOS HOMBRES 

# Nomenclatura: chances de la variable Y de la categoria 1 de X es beta veces mayor o menor que la de categoria de referencia de X

# Esto es similar a la comparacion de los odss de dos medias condicionales 

# Propiedades Odss ratio

# Odss de 1 significan chances iguales 

# Rango de posibles valores es de 0 < Oddsratio < infinito

# Cuando los valores van de 0 a 1, Oddsratio indica odss1 < odss2
# Cuando los valores van de 1 a infinto, Oddsratio indica odss1 > odss2

# Son proprociones simetricas: un odd = 4 positivo es proprocional a una asociacion negativa de Odd = 1/4 = 0.25

explanatory = "sex"
dependent = "survived"

tt %>% or_plot(dependent, explanatory) # grafica odss ratio 

# Oddsratio de sobrevivienda de las mujeres por sobre los hombres (11.78). Indica tambien la cercania o lejania del cero (rango de asociacion negativa)  
