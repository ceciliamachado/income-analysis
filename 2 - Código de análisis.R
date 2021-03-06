#=====================================================================================================
# Universidad ORT Uruguay
# Facultad de Administraci�n y Ciencias Sociales
# Obligatorio de Anal�tica de Negocios y Big Data
# Docente: Mag. Guillermo Magnou
# Cecilia Machado - N�213640
#=====================================================================================================
#************
# PARTE UNO
#************
#========================================================================================
# Inicio de pre�mbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librer�as
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

#Fin de pre�mbulo
#======================================================================

#***********************
# An�lisis descriptivo
#***********************

# Histogramas

hist(datos$Ingreso, col = "brown3", main = "Histograma de Ingreso", xlab = "Ingreso", ylab = "Frecuencia absoluta" )
hist(datos$Ant.Lab, col = "brown3", main = "Histograma de Antig�edad Laboral", xlab = "Antig�edad Laboral", ylab = "Frecuencia absoluta" )
hist(datos$Hab, col = "brown3", main = "Histograma de Habitantes", xlab = "Habitantes", ylab = "Frecuencia absoluta" )
hist(datos$Edu.Ter, col = "brown3", main = "Histograma de Educaci�n Terciaria", xlab = "Educaci�n Terciaria", ylab = "Frecuencia absoluta" )

# Diagrama de cajas

boxplot(datos$Ingreso, col = "brown3", main = "Diagrama de caja de Ingreso", ylab="Ingreso" )
boxplot(datos$Ant.Lab,col = "brown3", main = "Diagrama de caja de Antig�edad Laboral", ylab="Ant.Lab" )
boxplot(datos$Hab, col = "brown3",main = "Diagrama de caja de Habitantes", ylab="Hab" )
boxplot(datos$Edu.Ter,col = "brown3", main = "Diagrama de caja de Educaci�n Terciaria", ylab="Edu.Ter" )

# Medidas de Resumen, separaci�n y dispersi�n:

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

# Desviaci�n est�ndar

Desviaci�n_Ant.Lab <- sd(datos$Ant.Lab)
Desviaci�n_Ant.Lab

Desviaci�n_Edu.Ter <- sd(datos$Edu.Ter)
Desviaci�n_Edu.Ter

Desviaci�n_Hab <- sd(datos$Hab)
Desviaci�n_Hab

Desviaci�n_Ingreso <- sd(datos$Ingreso)
Desviaci�n_Ingreso

lista_desvio <- matrix(c(Desviaci�n_Ant.Lab, Desviaci�n_Edu.Ter, Desviaci�n_Hab, Desviaci�n_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_desvio) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_desvio) <- c("Desviaci�n est�ndar")
lista_desvio <- as.table(lista_desvio)
lista_desvio

# Coeficiente de variaci�n

CoefVariacion_Ant.Lab <- (Desviaci�n_Ant.Lab /mean(datos$Ant.Lab ))*100
CoefVariacion_Ant.Lab   

CoefVariacion_Edu.Ter <- (Desviaci�n_Edu.Ter/mean(datos$Edu.Ter))*100
CoefVariacion_Edu.Ter  

CoefVariacion_Hab <- (Desviaci�n_Hab/mean(datos$Hab))*100
CoefVariacion_Hab 

CoefVariacion_Ingreso <- (Desviaci�n_Ingreso/mean(datos$Ingreso))*100
CoefVariacion_Ingreso  

lista_cv <- matrix(c(CoefVariacion_Ant.Lab, CoefVariacion_Edu.Ter, CoefVariacion_Hab, CoefVariacion_Ingreso),ncol=4,byrow=TRUE)
colnames(lista_cv) <- c("Ant.Lab","Edu.Ter","Hab", "Ingreso")
rownames(lista_cv) <- c("Desviaci�n est�ndar")
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

# Rango Intercualt�lico

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
rownames(lista_final) <- c("Varianza", "Desviaci�n Est�ndar", "Coef. Variaci�n","Rango", "RIC")
lista_final <- as.table(lista_final)
lista_final

# Creaci�n de tabla de frecuencias de clases de la variable Ingreso

# Ancho de clases = (max - min)/N�mero de clases

(2.5-0.1)/3

# Ancho de clases = 0.8

val_ini_Ingreso <- 0.1
val_fin_Ingreso <- 2.5
salto_Ingreso   <-  0.8
clasesIngreso   <- seq(val_ini_Ingreso,val_fin_Ingreso,salto_Ingreso)
clasesIngreso

# Se genera una variable tal que cada valor sea a qu� clase pertenece cada observaci�n de Ingreso
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
#An�lisis de correlaci�n.
#***********************

tabla_correlaci�n <- cor(datos[,2:5])
tabla_correlaci�n

#Gr�ficos de dispersi�n.

Graf_Ingreso_vs_AntLab <- plot(datos$Ant.Lab, datos$Ingreso, col = "brown2", main = 'Ingreso vs Ant. Lab', xlab = 'Ingreso', ylab = 'Antiguedad Laboral')
abline(lm(Ingreso~Ant.Lab, data = datos)) 

Graf_Ingreso_vs_Hab <- plot(datos$Ingreso, datos$Hab , col = 'brown2', main = 'Ingreso vs Hab', xlab = 'Ingreso', ylab = 'Habitantes')
abline(lm(Hab~Ingreso, data = datos))

Graf_Ingreso_vs_EduTer <- plot(datos$Ingreso, datos$Edu.Ter , col = 'brown2', main = 'Ingreso vs Edu Ter', xlab = 'Ingreso', ylab = 'Educaci�n Terciaria')
abline(lm(Edu.Ter~Ingreso, data = datos)) 

#*************************
#Regresi�n lineal m�ltiple
#*************************

#Creamos las variables dummies para la variable categ�rica Nivel S.E

datos$Bajo_dum <- ifelse(datos$Nivel.SE == 'Bajo', 1, 0)
table(datos$Bajo_dum, datos$Nivel.SE)

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)

#Creamos el dataset de traininig y testing

set.seed(1111)

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos), nrow(datos)*0.7)
test <- (-train)

#Creamos regresi�n lineal con todas las variables
reg1   <- lm(Ingreso ~ Ant.Lab+Hab+Edu.Ter+Bajo_dum+Alto_dum , data = datos,subset = train) 

summary(reg1)

vif(reg1)

#Trabajamos con un nivel de significancia, ?? = 0.10

#Regresi�n lineal aplicando m�todo autom�tico
reg1.step <- step(reg1, direction = "backward")

#Creamos regresi�n lineal sin las variables: Ant.Lab y Edu.Ter

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

# Interpretaci�n del modelo

# Predecimos el ingreso de una persona con una cantidad de 3 Habitantes, y nivel socioecon�mico Alto

0.57806+0.27217*3-1.26481*0+0.66290*1

# Predecimos que una persona con las caracter�sticas mencionadas anteriormente tendr�a un ingreso de $20.574,7

#============================================================================================

#************
#PARTE DOS
#************

#============================================================================================
# Inicio de pre�mbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librer�as
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

#Fin de pre�mbulo
#======================================================================

#*****************************
#Modelo de Regresi�n Log�stica
#*****************************

#Tabla de frecuencias absolutas de variable categ�rica Nivel S.E Vs Ingreso

datos$Ing_cat = ifelse(datos$Ingreso <1 , "Menor a 1", ifelse(datos$Ingreso >2, "Mayor a 2", "Entre 1 y 2"))

tabla_frec_abs_IngVSNivelSE <- table(datos$Ing_cat, datos$Nivel.SE)
tabla_frec_abs_IngVSNivelSE

tabla_frec_abs_IngVSNivelSE_marg <- addmargins(tabla_frec_abs_IngVSNivelSE)
tabla_frec_abs_IngVSNivelSE_marg

#Tabla de frecuencias porcentuales de la variable categ�rica Nivel S.E Vs Ingreso

tabla_frec_relPor_IngVSNivelSE <- addmargins(prop.table(table(datos$Ing_cat, datos$Nivel.SE), 1), 2)*100
tabla_frec_relPor_IngVSNivelSE

tabla_frec_relPor_IngVSNivelSE_red <- round(tabla_frec_relPor_IngVSNivelSE)
tabla_frec_relPor_IngVSNivelSE_red


#Creamos la variable dummy para la variable categ�rica Nivel S.E

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)
addmargins(table(datos$Alto_dum, datos$Nivel.SE))


# Diagrama de caja e histograma

boxplot(datos$Edu.Ter~datos$Alto_dum, xlab = 'Nivel socioecon�mico: 0 = Bajo y Medio, 1 = Alto',
ylab = 'Educaci�n terciaria (a�os)',main = 'Edu.Ter respecto al Nivel.SE',col= c("brown4", "brown2"))

boxplot(datos$Ingreso~datos$Alto_dum, xlab = 'Nivel socioecon�mico: 0 = Bajo y Medio, 1 = Alto',
        ylab = 'Ingreso ($)',main = 'Ingresos respecto al Nivel.SE',col= c("brown4", "brown2"))

par(mfrow = c(2, 2))
hist(datos$Edu.Ter[datos$Alto_dum == 1], col = 'brown2', main = 'Edu.Ter y Nivel.SE Alto', ylab = "Frecuencia", xlab = "Educaci�n terciaria (a�os)")
hist(datos$Edu.Ter[datos$Alto_dum == 0], col = 'brown4', main = 'Edu.Ter y Nivel.SE Medio y Bajo', ylab = "Frecuencia", xlab = "Educaci�n terciaria (a�os)")

hist(datos$Ingreso[datos$Alto_dum == 1], col = 'brown2', main = 'Ingreso y Nivel.SE Alto', ylab = "Frecuencia", xlab = "Ingreso ($)")
hist(datos$Ingreso[datos$Alto_dum == 0], col = 'brown4', main = 'Ingreso y Nivel.SE Medio y Bajo', ylab = "Frecuencia", xlab = "Ingreso ($)")


#Creamos el dataset de traininig y testing

set.seed(1111)

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos), nrow(datos)*0.7)
test <- (-train)


# Regresi�n Log�stica con todas las variables

glm.fit <- glm(Alto_dum ~ Ingreso + Ant.Lab + Hab + Edu.Ter , datos[train, ], family=binomial)

summary(glm.fit)

# Regresi�n Log�stica aplicando m�todo atutom�tico

glm.fit.back <- step(glm.fit, direction = 'backward')

summary(glm.fit.back)


# Regresi�n log�stica final sin Hab (nivel de significancia mayor que 0.10)

glm.fit2 <- glm(Alto_dum ~ Ingreso + Edu.Ter , datos[train, ], family=binomial)

summary(glm.fit2)

vif(glm.fit2)

#Predicci�n en toda la base de datos

datos$Alto_dum_pred <- predict(glm.fit2, datos, type ="response")

summary(datos$Alto_dum_pred)

#Predicci�n en test

datos$Alto_dum_pred_test <- ifelse(datos$Alto_dum_pred > 0.5, 1, 0)
table(datos$Alto_dum_pred_test[test])
addmargins(table(datos$Alto_dum_pred_test[test], datos$Alto_dum[test]))

#Evaluaci�n en test - VER VALORES

# Precisi�n 
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
# Inicio de pre�mbulo

# Borrar toda al memoria de trabajo
rm(list=ls()) 

# Cargamos librer�as

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


#Fin de pre�mbulo
#======================================================================

#********************************
#Modelo de �rbol de clasificaci�n
#********************************

#Creamos las variables dummies para la variable categ�rica Nivel S.E

datos$Alto_dum <- ifelse(datos$Nivel.SE == 'Alto', 1, 0)
table(datos$Alto_dum, datos$Nivel.SE)

#Creamos el dataset de traininig y testing

set.seed(1111)

#Sacamos las variables individuales ID y Nivel S.E

datos2 <- datos [,-c(1,6)]

#Separamos el dataset en 70% para train y 30% para test

train <- sample(nrow(datos2), nrow(datos2)*0.7)
test <- (-train)

#Estimamos nuestro primer modelo en training, con el m�todo "class"

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

# Precisi�n
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

# Inicio de pre�mbulo

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

#Fin de pre�mbulo
#======================================================================

#**************
#Modelo K-Means
#**************

# Sacamos las variables ID y NivelSE del dataset

datoskm <- datos[,2:5]
view(datoskm)
summary(datoskm)

# Justificaci�n de cantidad de clusters
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

# Visualizamos la informaci�n de cada cluster

NSE_c[NSE_c$cluster==1,]
NSE_c[NSE_c$cluster==2,]
NSE_c[NSE_c$cluster==3,]


# Interpretaci�n gr�fica de los clusters
par(mfrow = c(2, 2))

boxplot(Ingreso~km_3_NSE$cluster,data = datos,main = 'Ingreso',xlab = "Clusters",ylab = "Ingreso($)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Ant.Lab~km_3_NSE$cluster,data = datos,main = 'Antig�edad laboral',xlab = "Clusters",ylab = "Ant.Lab(a�os)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Edu.Ter~km_3_NSE$cluster,data = datos,main = 'Educaci�n terciaria',xlab = "Clusters",ylab = "Edu.Ter(a�os)",col = c('brown1', 'brown2', 'brown4'))
boxplot(Hab~km_3_NSE$cluster,data = datos,main = 'Habitantes',xlab = "Clusters",ylab = "Hab(cant.prom)",col = c('brown1', 'brown2', 'brown4'))


# Gr�fico con centroides

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

