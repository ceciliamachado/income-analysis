#=====================================================================================================
# Universidad ORT Uruguay
# Facultad de Administración y Ciencias Sociales
# Obligatorio de Analítica de Negocios y Big Data
# Docente: Mag. Guillermo Magnou
# Cecilia Machado - N°213640
#=====================================================================================================
#************
# PARTE UNO
#************
#========================================================================================
# Inicio de preámbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librerías
library(rio)
library(tidyverse)
library(ggplot2)
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(funModeling)
library(fastDummies)
library(ISLR)

# Establecemos el directorio de trabajo
setwd('C:/Users/Cecilia Machado/Desktop/Obligatorio Analitica de Negocios y Big Data')

#Chequeamos que haya sido correctamente ejecutado
getwd()

# Cargamos base de datos 
datos <- import('base.csv')
# Chequeamos que la cantidad de filas y columnas sean los correctos 
dim(datos)
# Visualizamos base de datos
View(datos)
# Visualizamos tipo de datos 
str(datos)
head(datos)

#Fin de preámbulo
#======================================================================

#***********************
# Análisis descriptivo
#***********************

# Histogramas

hist(datos$Ingreso, col = "brown3", main = "Histograma de Ingreso", xlab = "Ingreso", ylab = "Frecuencia absoluta" )
hist(datos$Ant.Lab, col = "brown3", main = "Histograma de Antigüedad Laboral", xlab = "Antigüedad Laboral", ylab = "Frecuencia absoluta" )
hist(datos$Hab, col = "brown3", main = "Histograma de Habitantes", xlab = "Habitantes", ylab = "Frecuencia absoluta" )
hist(datos$Edu.Ter, col = "brown3", main = "Histograma de Educación Terciaria", xlab = "Educación Terciaria", ylab = "Frecuencia absoluta" )

# Diagrama de cajas

boxplot(datos$Ingreso, col = "brown3", main = "Diagrama de caja de Ingreso", ylab="Ingreso" )
boxplot(datos$Ant.Lab,col = "brown3", main = "Diagrama de caja de Antigüedad Laboral", ylab="Ant.Lab" )
boxplot(datos$Hab, col = "brown3",main = "Diagrama de caja de Habitantes", ylab="Hab" )
boxplot(datos$Edu.Ter,col = "brown3", main = "Diagrama de caja de Educación Terciaria", ylab="Edu.Ter" )

# Medidas de Resumen, separación y dispersión:

# Resumen de las variables del archivo

summary(datos[,2:6])

# Varianza de variables cuantitativas

Varianza_Ant.Lab <- var(datos$Ant.Lab)
Varianza_Ant.Lab

Varianza_Hab <- var(datos$Hab)
Varianza_Hab

Varianza_Edu.Ter <- var(datos$Edu.Ter)
Varianza_Edu.Ter

Varianza_Ingreso <- var(datos$Ingreso) 
Varianza_Ingreso

lista_varianza <- matrix(c(Varianza_Ant.Lab, Varianza_Edu.Ter, Varianza_Hab, Varianza_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_varianza) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_varianza) <- c("Varianza")
lista_varianza <- as.table(lista_varianza)
lista_varianza

# Desviación estándar

Desviación_Ant.Lab <- sd(datos$Ant.Lab)
Desviación_Ant.Lab

Desviación_Edu.Ter <- sd(datos$Edu.Ter)
Desviación_Edu.Ter

Desviación_Hab <- sd(datos$Hab)
Desviación_Hab

Desviación_Ingreso <- sd(datos$Ingreso)
Desviación_Ingreso

lista_desvio <- matrix(c(Desviación_Ant.Lab, Desviación_Edu.Ter, Desviación_Hab, Desviación_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_desvio) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_desvio) <- c("Desviación estándar")
lista_desvio <- as.table(lista_desvio)
lista_desvio

# Coeficiente de variación

CoefVariacion_Ant.Lab <- (Desviación_Ant.Lab /mean(datos$Ant.Lab ))*100
CoefVariacion_Ant.Lab   

CoefVariacion_Edu.Ter <- (Desviación_Edu.Ter/mean(datos$Edu.Ter))*100
CoefVariacion_Edu.Ter  

CoefVariacion_Hab <- (Desviación_Hab/mean(datos$Hab))*100
CoefVariacion_Hab 

CoefVariacion_Ingreso <- (Desviación_Ingreso/mean(datos$Ingreso))*100
CoefVariacion_Ingreso  

lista_cv <- matrix(c(CoefVariacion_Ant.Lab, CoefVariacion_Edu.Ter, CoefVariacion_Hab, CoefVariacion_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_cv) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_cv) <- c("Desviación estándar")
lista_cv <- as.table(lista_cv)
lista_cv

# Rango 

Rango_Ant.Lab <- (max(datos$Ant.Lab))-(min(datos$Ant.Lab))
Rango_Ant.Lab

Rango_Edu.Ter <- (max(datos$Edu.Ter))-(min(datos$Edu.Ter))
Rango_Edu.Ter

Rango_Hab <- (max(datos$Hab))-(min(datos$Hab))
Rango_Hab

Rango_Ingreso <- (max(datos$Ingreso))-(min(datos$Ingreso))
Rango_Ingreso

lista_Rango <- matrix(c(Rango_Ant.Lab, Rango_Edu.Ter, Rango_Hab, Rango_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_Rango) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_Rango) <- c("Rango")
lista_Rango <- as.table(lista_Rango)
lista_Rango

# Rango Intercualtílico

RIC_Ant.Lab <- IQR(datos$Ant.Lab)
RIC_Ant.Lab

RIC_Edu.Ter <- IQR(datos$Edu.Ter)
RIC_Edu.Ter

RIC_Hab <- IQR(datos$Hab)
RIC_Hab

RIC_Ingreso <- IQR(datos$Ingreso)
RIC_Ingreso

lista_RIC <- matrix(c(RIC_Ant.Lab, RIC_Edu.Ter, RIC_Hab, RIC_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_RIC) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_RIC) <- c("RIC")
lista_RIC <- as.table(lista_RIC)
lista_RIC


# Lista final con datos obtenidos

lista_final <- matrix(c(lista_varianza, lista_desvio, lista_cv, lista_RIC, lista_Rango),ncol=4,byrow=TRUE)
colnames(lista_final) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_final) <- c("Varianza", "Desviación Estándar", "Coef. Variación","Rango", "RIC")
lista_final <- as.table(lista_final)
lista_final

# Creación de tabla de frecuencias de clases de la variable Ingreso

# Ancho de clases = (max - min)/Número de clases

(2.5-0.1)/3

# Ancho de clases = 0.8

val_ini_Ingreso <- 0.1
val_fin_Ingreso <- 2.5
salto_Ingreso   <-  0.8
clasesIngreso   <- seq(val_ini_Ingreso,val_fin_Ingreso,salto_Ingreso)
clasesIngreso

# Se genera una variable tal que cada valor sea a qué clase pertenece cada observación de Ingreso
clases_Ingreso  <- cut(datos$Ingreso, breaks = clasesIngreso)
print(clases_Ingreso)

# A esa nueva variable, calcularle las frecuencias absolutas:
Frec_Abs_Clases_Ingreso      <- table(clases_Ingreso)
Frec_Abs_Clases_Ingreso

# Expresarlo como un "data frame" que es nuestra tabla deseada a la que le vamos a ir
# agregando columnas

tabla_frecuencia_clases_Ingreso <- data.frame(Frec_Abs_Clases_Ingreso)
tabla_frecuencia_clases_Ingreso

# Agregar al data frame una columna de Frecuencias relativas

Frec_rel_Ingreso        <- tabla_frecuencia_clases_Ingreso$Freq/sum(tabla_frecuencia_clases_Ingreso$Freq)
tabla_frecuencia_clases_Ingreso$Frec_rel_Ingreso <- Frec_rel_Ingreso
tabla_frecuencia_clases_Ingreso

# Agregar al data frame una columna de Frecuencias porcentuales

Frec_por_Ingreso        <- tabla_frecuencia_clases_Ingreso$Freq/sum(tabla_frecuencia_clases_Ingreso$Freq)*100
tabla_frecuencia_clases_Ingreso$Frec_por_Ingreso <- Frec_por_Ingreso
tabla_frecuencia_clases_Ingreso

#***********************
#Análisis de correlación.
#***********************

tabla_correlación <- cor(datos[,2:5])
tabla_correlación

#Gráficos de dispersión.

Graf_Ingreso_vs_AntLab <- plot(datos$Ant.Lab, datos$Ingreso, col = "brown2", main = 'Ingreso vs Ant. Lab', xlab = 'Ingreso', ylab = 'Antiguedad Laboral')
abline(lm(Ingreso~Ant.Lab, data = datos)) 

Graf_Ingreso_vs_Hab <- plot(datos$Ingreso, datos$Hab , col = 'brown2', main = 'Ingreso vs Hab', xlab = 'Ingreso', ylab = 'Habitantes')
abline(lm(Hab~Ingreso, data = datos))

Graf_Ingreso_vs_EduTer <- plot(datos$Ingreso, datos$Edu.Ter , col = 'brown2', main = 'Ingreso vs Edu Ter', xlab = 'Ingreso', ylab = 'Educación Terciaria')
abline(lm(Edu.Ter~Ingreso, data = datos)) 

#*************************
#Regresión lineal múltiple
#*************************

#Creamos las variables dummies para la variable categórica Nivel S.E

datos$Bajo_dum <- ifelse(datos$Nivel.SE == 'Bajo', 1, 0)
table(datos$Bajo_dum, datos$Nivel.SE)

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)

#Creamos el dataset de traininig y testing

set.seed(1111)

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos), nrow(datos)*0.7)
test <- (-train)

#Creamos regresión lineal con todas las variables
reg1   <- lm(Ingreso ~ Ant.Lab+Hab+Edu.Ter+Bajo_dum+Alto_dum , data = datos,subset = train) 

summary(reg1)

vif(reg1)

#Trabajamos con un nivel de significancia, ?? = 0.10

#Regresión lineal aplicando método automático
reg1.step <- step(reg1, direction = "backward")

#Creamos regresión lineal sin las variables: Ant.Lab y Edu.Ter

reg2  <- lm(Ingreso ~ Hab+Bajo_dum+Alto_dum , data = datos,subset = train) 

summary(reg2)

vif(reg2)

#ECM en testing

mean((datos$Ingreso[test] - predict(reg2, datos[test, ]))**2)

# R-cuadrado en testing

corel <- cor(datos$Ingreso[test], predict(reg2, datos[test, ]))
corel**2

#ECM en train

mean((datos$Ingreso[train] - predict(reg2, datos[train, ]))**2)

# R-cuadrado en train

corel <- cor(datos$Ingreso[train], predict(reg2, datos[train, ]))
corel**2

# Interpretación del modelo

# Predecimos el ingreso de una persona con una cantidad de 3 Habitantes, y nivel socioeconómico Alto

0.57806+0.27217*3-1.26481*0+0.66290*1

# Predecimos que una persona con las características mencionadas anteriormente tendría un ingreso de $20.574,7

#============================================================================================

#************
#PARTE DOS
#************

#============================================================================================
# Inicio de preámbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librerías
library(rio)
library(tidyverse)
library(ggplot2)
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(funModeling)
library(fastDummies)
library(ISLR)

# Establecemos el directorio de trabajo
setwd('C:/Users/Cecilia Machado/Desktop/Obligatorio Analitica de Negocios y Big Data')
#Chequeamos que haya sido correctamente ejecutado
getwd()
# Cargamos base de datos 
datos <- import('base.csv')
# Chequeamos que la cantidad de filas y columnas sean los correctos 
dim(datos)
# Visualizamos base de datos
View(datos)
# Visualizamos tipo de datos 
str(datos)
head(datos)

#Fin de preámbulo
#======================================================================

#*****************************
#Modelo de Regresión Logística
#*****************************

#Tabla de frecuencias absolutas de variable categórica Nivel S.E Vs Ingreso

datos$Ing_cat = ifelse(datos$Ingreso <1 , "Menor a 1", ifelse(datos$Ingreso >2, "Mayor a 2", "Entre 1 y 2"))

tabla_frec_abs_IngVSNivelSE <- table(datos$Ing_cat, datos$Nivel.SE)
tabla_frec_abs_IngVSNivelSE

tabla_frec_abs_IngVSNivelSE_marg <- addmargins(tabla_frec_abs_IngVSNivelSE)
tabla_frec_abs_IngVSNivelSE_marg

#Tabla de frecuencias porcentuales de la variable categórica Nivel S.E Vs Ingreso

tabla_frec_relPor_IngVSNivelSE <- addmargins(prop.table(table(datos$Ing_cat, datos$Nivel.SE), 1), 2)*100
tabla_frec_relPor_IngVSNivelSE

tabla_frec_relPor_IngVSNivelSE_red <- round(tabla_frec_relPor_IngVSNivelSE)
tabla_frec_relPor_IngVSNivelSE_red


#Creamos la variable dummy para la variable categórica Nivel S.E

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)
addmargins(table(datos$Alto_dum, datos$Nivel.SE))


# Diagrama de caja e histograma

boxplot(datos$Edu.Ter~datos$Alto_dum, xlab = 'Nivel socioeconómico: 0 = Bajo y Medio, 1 = Alto',
ylab = 'Educación terciaria (años)',main = 'Edu.Ter respecto al Nivel.SE',col= c("brown4", "brown2"))

boxplot(datos$Ingreso~datos$Alto_dum, xlab = 'Nivel socioeconómico: 0 = Bajo y Medio, 1 = Alto',
        ylab = 'Ingreso ($)',main = 'Ingresos respecto al Nivel.SE',col= c("brown4", "brown2"))

par(mfrow = c(2, 2))
hist(datos$Edu.Ter[datos$Alto_dum == 1], col = 'brown2', main = 'Edu.Ter y Nivel.SE Alto', ylab = "Frecuencia", xlab = "Educación terciaria (años)")
hist(datos$Edu.Ter[datos$Alto_dum == 0], col = 'brown4', main = 'Edu.Ter y Nivel.SE Medio y Bajo', ylab = "Frecuencia", xlab = "Educación terciaria (años)")

hist(datos$Ingreso[datos$Alto_dum == 1], col = 'brown2', main = 'Ingreso y Nivel.SE Alto', ylab = "Frecuencia", xlab = "Ingreso ($)")
hist(datos$Ingreso[datos$Alto_dum == 0], col = 'brown4', main = 'Ingreso y Nivel.SE Medio y Bajo', ylab = "Frecuencia", xlab = "Ingreso ($)")


#Creamos el dataset de traininig y testing

set.seed(1111)

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos), nrow(datos)*0.7)
test <- (-train)


# Regresión Logística con todas las variables

glm.fit <- glm(Alto_dum ~ Ingreso + Ant.Lab + Hab + Edu.Ter , datos[train, ], family=binomial)

summary(glm.fit)

# Regresión Logística aplicando método atutomático

glm.fit.back <- step(glm.fit, direction = 'backward')

summary(glm.fit.back)


# Regresión logística final sin Hab (nivel de significancia mayor que 0.10)

glm.fit2 <- glm(Alto_dum ~ Ingreso + Edu.Ter , datos[train, ], family=binomial)

summary(glm.fit2)

vif(glm.fit2)

#Predicción en toda la base de datos

datos$Alto_dum_pred <- predict(glm.fit2, datos, type ="response")

summary(datos$Alto_dum_pred)

#Predicción en test

datos$Alto_dum_pred_test <- ifelse(datos$Alto_dum_pred > 0.5, 1, 0)
table(datos$Alto_dum_pred_test[test])
addmargins(table(datos$Alto_dum_pred_test[test], datos$Alto_dum[test]))

#Evaluación en test - VER VALORES

# Precisión 
(12+32)/45
# Especificidad
32/32
# Sensivilidad 
12/13
# Tasa de error
(0+1)/45

# Curva ROC

rocobj <- roc( datos$Alto_dum[test], datos$Alto_dum_pred_test[test], auc = TRUE, ci = TRUE  )
print(rocobj)

plot.roc( rocobj, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
          auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
          col = 2, grid = TRUE )

# Multicolinealidad

cor(datos$Ingreso,datos$Edu.Ter)

vif(glm.fit2)


#======================================================================
# Inicio de preámbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librerías

library(rio)
library(rpart)
library(rattle) 
library(corrplot)
library(pROC)


# Establecemos el directorio de trabajo
setwd('C:/Users/Cecilia Machado/Desktop/Obligatorio Analitica de Negocios y Big Data')
#Chequeamos que haya sido correctamente ejecutado
getwd()
# Cargamos base de datos 
datos <- import('base.csv')
# Chequeamos nombre de las variables
names(datos)
# Chequeamos que la cantidad de filas y columnas sean los correctos 
dim(datos)
# Visualizamos base de datos
View(datos)
# Visualizamos tipo de datos 
str(datos)
head(datos)

#Resumen de los datos
summary(datos)


#Fin de preámbulo
#======================================================================

#********************************
#Modelo de Árbol de clasificación
#********************************

#Creamos las variables dummies para la variable categórica Nivel S.E

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)

#Creamos el dataset de traininig y testing

set.seed(1111)

#Sacamos las variables individuales ID y Nivel S.E

datos2 <- datos [,-c(1,6)]

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos2), nrow(datos2)*0.7)
test <- (-train)

#Estimamos nuestro primer modelo en training, con el método "class"

arbol.inicial <- rpart(Alto_dum ~ Ant.Lab + Hab + Edu.Ter + Ingreso , datos2[train, ], method = 'class')
arbol.inicial

#Graficamos nuestro arbol inicial
fancyRpartPlot(arbol.inicial)

#Graficamos la performance del modelo vs. la complejidad
plotcp(arbol.inicial)

#Ver reglas
asRules(arbol.inicial)

#Hacemos la prediccion del modelo sobre todo el dataset

datos2$pred_arbol_NSEAlto = predict(arbol.inicial, datos2)[,2]
summary(datos2$pred_arbol_NSEAlto)


# Curva ROC en test

roc_test <- roc(datos2$Alto_dum[test], datos2$pred_arbol_NSEAlto [test], auc = TRUE, ci = TRUE  )
print(roc_test)

plot.roc( roc_test, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
          auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
          col = 2, grid = TRUE )

# Curva ROC en train

roc_train <- roc(datos2$Alto_dum[train], datos2$pred_arbol_NSEAlto [train], auc = TRUE, ci = TRUE  )
print(roc_train)

plot.roc( roc_train, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
          auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
          col = 2, grid = TRUE )

# Tabla de confusion con probabilidad > 0.5 como 'punto de corte'
addmargins(table(datos2$pred_arbol_NSEAlto[test] > 0.5, datos2$Alto_dum[test]))

# Precisión
(13+30)/45
# Sensibilidad
13/13
# Especificidad
30/32 
# Tasa de error
(0+2)/45

#=========================================================================

#************
#PARTE TRES
#************

#=========================================================================

# Inicio de preámbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

library(rio)
library(tidyverse)
library(ggplot2)
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(funModeling)
library(fastDummies)
library(ISLR)

# Establecemos el directorio de trabajo
setwd('C:/Users/Cecilia Machado/Desktop/Obligatorio Analitica de Negocios y Big Data')
#Chequeamos que haya sido correctamente ejecutado
getwd()
# Cargamos base de datos 
datos <- import('base.csv')
# Chequeamos nombre de las variables
names(datos)
# Chequeamos que la cantidad de filas y columnas sean los correctos 
dim(datos)
# Visualizamos base de datos
View(datos)
# Visualizamos tipo de datos 
str(datos)
head(datos)

#Resumen de los datos
summary(datos)

#Fin de preámbulo
#======================================================================

#**************
#Modelo K-Means
#**************

# Sacamos las variables ID y NivelSE del dataset

datoskm <- datos[,2:5]
view(datoskm)
summary(datoskm)

# Justificación de cantidad de clusters
# Calculamos el WSS para distintos valores de K desde 1 hasta 8

wcv <- sapply(1:8 , function(k){kmeans(datoskm,k,nstart=25, iter.max=8)$tot.withinss})
wcv

plot(1:8, wcv, type="b", main = 'Seleccionar K', xlab = "Cantidad Clusters (K)", ylab = "WCV")

# Cluster con K=3
set.seed(123)

km_3_NSE <- kmeans(scale(datoskm), 3, nstart = 25)
km_3_NSE

# Cantidad de observaciones por clusters

table(km_3_NSE$cluster)

# Centroides con K=3
Centroides <- aggregate(datoskm, by=list(cluster=km_3_NSE$cluster), mean)

# Incorporo los cluster a los datos
NSE_c <- cbind(datoskm, cluster = km_3_NSE$cluster)

# Visualizamos la información de cada cluster

NSE_c[NSE_c$cluster==1,]
NSE_c[NSE_c$cluster==2,]
NSE_c[NSE_c$cluster==3,]


# Interpretación gráfica de los clusters
par(mfrow = c(2, 2))

boxplot(Ingreso~km_3_NSE$cluster,data = datos,main = 'Ingreso',xlab = "Clusters",ylab = "Ingreso($)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Ant.Lab~km_3_NSE$cluster,data = datos,main = 'Antigüedad laboral',xlab = "Clusters",ylab = "Ant.Lab(años)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Edu.Ter~km_3_NSE$cluster,data = datos,main = 'Educación terciaria',xlab = "Clusters",ylab = "Edu.Ter(años)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Hab~km_3_NSE$cluster,data = datos,main = 'Habitantes',xlab = "Clusters",ylab = "Hab(cant.prom)",col = c('brown1', 'brown2', 'brown4'))


# Gráfico con centroides

vcol <- c('brown4', 'red2', "lightsalmon")

plot(datoskm$Edu.Ter, datoskm$Ingreso, main = "Centroides K Means con K=3", xlab = "Ingreso", ylab = "Edu.Ter", 
     col= vcol[km_3_NSE$cluster], pch = 20, cex = 1)
points(Centroides$Edu.Ter, Centroides$Ingreso, col= vcol , pch = 8, cex = 2)

plot(datoskm$Ant.Lab, datoskm$Ingreso, main = "Centroides K Means con K=3", xlab = "Ingreso", ylab = "Ant.Lab", 
    col= vcol[km_3_NSE$cluster], pch = 20, cex = 1)
points(Centroides$Ant.Lab, Centroides$Ingreso, col= vcol, pch = 8, cex = 2)

plot(datoskm$Hab, datoskm$Ingreso, main = "Centroides K Means con K=3", xlab = "Ingreso", ylab = "Hab", 
     col= vcol[km_3_NSE$cluster], pch = 20, cex = 1)
points(Centroides$Hab, Centroides$Ingreso, col = vcol , pch = 8, cex = 2)

