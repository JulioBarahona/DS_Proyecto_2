"
An�lisis explicativo para   comportamiento de Lepidoptera Heterocera bajo diferentes longitudes de onda
Julio Barahona M , 141206
Kevin ****, ****
Manuela Pacheco , ----
"
#librerias usadas
library(corrplot)
library(cluster)
library(e1071)
library(mclust) 
library(fpc) 
library(tidyr) 
library(readxl)
library(car)
library(rpart)
library(caret)
library(tree)
require(rpart)
library(randomForest)
library(dplyr)
library(ggplot2)
library(reshape2)

#Cargar datos
palomilla_01 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/01.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_02 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/02.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_03 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/03.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_04 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/04.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_05 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/05.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_06 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/06.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_07 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/07.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_08 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/08.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_09 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/09.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_10 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/10.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_11 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/11.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_12 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/12.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_13 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/13.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_14 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/14.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_15 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/15.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_16 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/16.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_17 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/17.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_18 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/18.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_19 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/19.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_20 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/20.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_21 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/21.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_22 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/22.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_23 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/23.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_24 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/24.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_25 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/25.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_26 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/26.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_27 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/27.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_28 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/28.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_29 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/29.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)
palomilla_30 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Primer Semestre/Data Mining/Proyecto/30.csv", header = TRUE , sep=",",stringsAsFactors = FALSE)

#Por recomendaciones se agreg� la temperatura y humerdad
palomilla_01$temperatura <- 16.17
palomilla_01$humedad <- 68.49
palomilla_02$temperatura <- 16.20
palomilla_02$humedad <- 68.38
palomilla_03$temperatura<-16.49
palomilla_03$temperatura <- 16.51
palomilla_03$humedad <- 69.18
palomilla_04$temperatura <- 16.53
palomilla_04$humedad <- 69.23
palomilla_05$temperatura <- 19.91
palomilla_05$humedad <- 69.30
palomilla_06$temperatura <- 19.84
palomilla_06$humedad <- 69.71
palomilla_07$temperatura <- 16.15
palomilla_07$humedad <- 67.98
palomilla_08$temperatura <- 21.51
palomilla_08$humedad <- 69.87
palomilla_09$temperatura <- 16.29
palomilla_09$humedad <- 68.78
palomilla_10$temperatura <- 20.46
palomilla_10$humedad <- 69.75
palomilla_11$temperatura <- 20.65
palomilla_11$humedad <- 69.58
palomilla_12$temperatura <- 23.32
palomilla_12$humedad <- 70.96
palomilla_13$temperatura <- 20.73
palomilla_13$humedad <- 69.27
palomilla_14$temperatura <- 20.32
palomilla_14$humedad <- 69.62
palomilla_15$temperatura <- 23.95
palomilla_15$humedad <- 70.81
palomilla_16$temperatura <- 21.21
palomilla_16$humedad <- 69.79
palomilla_17$temperatura <- 19.52
palomilla_17$humedad <- 69.32
palomilla_18$temperatura <- 19.73
palomilla_18$humedad <- 69.81
palomilla_19$temperatura <- 19.81
palomilla_19$humedad <- 69.09
palomilla_20$temperatura <- 21.00
palomilla_20$humedad <- 69.84
palomilla_21$temperatura <- 24.41
palomilla_21$humedad <- 70.32
palomilla_22$temperatura <- 22.98
palomilla_22$humedad <- 70.31
palomilla_23$temperatura <- 20.26
palomilla_23$humedad <- 69.20
palomilla_24$temperatura <- 20.15
palomilla_24$humedad <- 69.72
palomilla_25$temperatura <- 20.26
palomilla_25$humedad <- 69.23
palomilla_26$temperatura <- 24.79
palomilla_26$humedad <- 71.03
palomilla_27$temperatura <- 21.46
palomilla_27$humedad <- 69.89
palomilla_28$temperatura <- 16.06
palomilla_28$humedad <- 67.88
palomilla_29$temperatura <- 21.93
palomilla_29$humedad <- 69.73
palomilla_30$temperatura <- 23.78
palomilla_30$humedad <- 70.28


#Juntar todos los datos en un solo df
raw_Todos <- rbind (palomilla_01,palomilla_02,palomilla_03,palomilla_04,palomilla_05,
                    palomilla_06,palomilla_07,palomilla_08,palomilla_09,palomilla_10,
                    palomilla_11,palomilla_12,palomilla_13,palomilla_14,palomilla_15,
                    palomilla_16,palomilla_17,palomilla_18,palomilla_19,palomilla_20,
                    palomilla_21,palomilla_22,palomilla_23,palomilla_24,palomilla_25,
                    palomilla_26,palomilla_27,palomilla_28,palomilla_29,palomilla_30)
#raw_Todos$familia <- as.factor(raw_Todos$familia)

#procesamiento de datos 
#0 arreglar los nombre de  Sphingidae y Nymphalidae
raw_Todos$familia <- raw_Todos[!(raw_Todos$familia=='Sphingidae ' |raw_Todos$familia=='Nymphalidae ') 

#1 pasar el porcetnaje de humedad a decimal
raw_Todos$humedad <-  raw_Todos$humedad/100

#2 dejar los mismos decimales 
is.num <- sapply(raw_Todos, is.numeric)
raw_Todos[is.num] <- lapply(raw_Todos[is.num], round, 4)

#3 eliminar la familia Sphingidae y Nymphalidae
nrow(raw_Todos[!(raw_Todos$familia=='Sphingidae ' |raw_Todos$familia=='Nymphalidae '),])

#resumen de las variables
sapply(raw_Todos, class)
summary(raw_Todos$familia)


#correlacion de las variables 
correlacion <- cor(raw_Todos[sapply(raw_Todos, is.numeric)])
View(as.data.frame(correlacion))
#se eliminana las variables que tiene alta correlacion mayor a 70%
raw_Todos$white_y <- NULL
raw_Todos$magenta_x <- NULL
raw_Todos$uv_ <- NULL
raw_Todos$cyan_x <- NULL
raw_Todos$magenta_y <- NULL
raw_Todos$humedad <- NULL
correlacion <- cor(raw_Todos[sapply(raw_Todos, is.numeric)])

#graficos
#correlaciones
corrplot(correlacion, method="number")

# Simple Bar Plot
counts <- table(raw_Todos$familia)
barplot(counts, main="Numero de muestras segun espec�menes",
        xlab="Numero de muestras") 

#box plot de datos por luz
hist(raw_Todos$red_y, breaks=6, col="blue")

#Clustering jerárquico
hc<-hclust(dist(raw_Todos[sapply(raw_Todos, is.numeric)])) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]

#Mixture of gaussians
mc<-Mclust(raw_Todos,5)
plot(mc, what = "classification", main="MClust Classification")
raw_Todos$mxGau<-mc$classification
g1MC<-raw_Todos[raw_Todos$mxGau==1,]
g2MC<-raw_Todos[raw_Todos$mxGau==2,]
g3MC<-raw_Todos[raw_Todos$mxGau==3,]
g3MC<-raw_Todos[raw_Todos$mxGau==4,]
g3MC<-raw_Todos[raw_Todos$mxGau==5,]


#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(iris[,1:4]))
mean(silkm[,3]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(iris[,1:4]))
mean(silch[,3]) #0.51, no es la mejor partición pero no está mal

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(iris[,1:4]))
mean(silfcm[,3]) #0.54, no es la mejor partición pero no está mal

#Método de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(iris[,1:4]))
mean(silmg[,3]) #0.50, no es la mejor partición pero no está mal

#Método de Ward para determinar el número correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(iris[,1:4])-1)*sum(apply(iris[,1:4],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(iris[,1:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
