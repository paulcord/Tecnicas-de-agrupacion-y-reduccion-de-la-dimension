### Práctica IRIS DATASET ###

#Cargamos la BBDD
data("iris")
data.frame(iris)

#Identificamos las variables y dimensiones
View(iris)
head(iris)
str(iris)
dim(iris)

#Análisis descriptivo variables
for (i in 1:4) {
  est<-summary(iris[, i])
  print(names(iris[i]))
  print(est)
}

#Histograma según variable
for (i in 1:4) {
  h <-hist(iris[,i], main=names(iris[i]),
           xlab="Tamaño", ylab="Frecuencia")
}

#Densidad según variable
for (i in 1:4) {
  density= density(iris[,i])
  plot(density, main = names(iris[i]))
}

#Diagrama de caja según especies
for (i in 1:4) {
  boxplot(iris[, i]~Species, data=iris, main=names(iris[i]),
          xlab="Especie",ylab="Tamaño")
}

#Instalación y carga de paquetes
install.packages("sm")
library(sm)
attach(iris)

#Densidad según especie (en conjunto) por variable 
especie.f <- factor(iris$Species, levels= c(1,2,3),
                labels = c("setosa", "versicolor", "virginica")) 

for (i in 1:4) {
  sm.density.compare(iris[,i], iris$Species, xlab="Tamaño")
  title(main=names(iris[i]))
}

#Gráfic dispersión según variables (distinguiendo especie)
pairs(iris[1:4], main = "Dispersión según especie",
      pch = 21, bg = c("red", "black", "brown")[unclass(iris$Species)])

#Análisis Discriminante Lineal
install.packages("MASS")
library(MASS)

ADL<- lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
          data=iris)                  
ADL

prediccion <- predict(object = ADL, newdata = iris[, 1:4])
str(prediccion)
table(iris$Species, prediccion$class, dnn = c("Clase True", "Clase Prediction"))

p.error<-(3/150)*100
p.error #El porcentaje de error es de 2%