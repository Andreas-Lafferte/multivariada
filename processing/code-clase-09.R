# Codigo practica 9: Regresion logistica

# 1. Cargar librerias ----

pacman::p_load(dplyr, summarytools, ggmosaic, finalfit)  

#  2. Cargar datos ---- 
load(url("https://multivariada.netlify.com/assignment/data/titanic.RData"))

# 3. Descriptivos ----
view(dfSummary(tt, headings = FALSE, method = "render")) 

# 4. Bivariados ----
ctable(tt$survived, tt$sex) # tablas de contingencia 

ggplot(data = tt) +
  geom_mosaic(aes(product(survived, sex), fill= survived)) + labs(y = "Sobreviviente", x = "Sexo") # grafica de mosaico

# 5. Conceptos centrales ----

## Odds 
## Ratio

# 5.1. Probabilidades ----

## Una probabilidad es la posibilidad de ocurrencia de un evento o suceso de interes dentro de un conjunto. 

## Podemos decir que del total de pasajeros, un 40.8% se ellos sobrevive = la probabildiad de sobrevivir es de 0.408

## Mientras que el 59.2% no sobrevive, por lo que la probabilidad de no sobrevivir = 0.592

prop.table(table(tt$survived))

# 5.2. Odds ---- 

## Un Odds se define como la division entre el numero de ocurrencias y el numero de no ocurrencias de un suceso.

## Odss = sobrevivientes/nosobrevivientes 

addmargins(table(tt$survived,tt$sex))

## Calculando los odss obtenemos 0.68 (427/619) = hay 0.68 sobreviviente por cada no sobreviviente. 

## Odss de sobreviviencia de hombres y mujeres
# odsshombres = 0.258 (odss menor a 1)
# odssmujeres = 3.04 (odss mayor a 1)

# Odss > 1 indican una chance positiva 
# Odss < 1 indican una chance negativa 

prop.table(table(tt$survived,tt$sex),2)

# 5.3. Odds Ratio (OR) ----

## ¿Cuanto mas sobreviven las mujeres que los hombres? Vale decir, ¿como se expresan las chances en terminos de los betas de regresion?  
## Esto implica la asociacion entre sobreviviena y sexo y ya no solo hablar de las chances de sobreviviencia de cada sexo por separado

# Odss Ratio (razon de chances)

# Odssratio = odssmujeres/odsshombres = 11.78

# INTERPRETACION: LAS CHANCES DE SOBREVIVIR DE LAS MUJERES ES 11.78 VECES MAS GRANDE QUE LA DE LOS HOMBRES 

# Nomenclatura: chances de la variable Y de la categoria 1 de X es beta veces mayor o menor que la de categoria de referencia de X

# Esto es similar a la comparacion de los odss de dos medias condicionales 

# 5.4. Propiedades Odss ratio ----

# Odss de 1 significan chances iguales 

# Rango de posibles valores es de 0 < Oddsratio < infinito

# Cuando los valores van de 0 a 1, Oddsratio indica odss1 < odss2
# Cuando los valores van de 1 a infinto, Oddsratio indica odss1 > odss2

# Son proprociones simetricas: un odd = 4 positivo es proprocional a una asociacion negativa de Odd = 1/4 = 0.25

explanatory = "sex"
dependent = "survived"

tt %>% or_plot(dependent, explanatory) # grafica odss ratio 

## Oddsratio de sobrevivienda de las mujeres por sobre los hombres (11.78) 
## Indica tambien la cercania o lejania del cero (rango de asociacion negativa)  