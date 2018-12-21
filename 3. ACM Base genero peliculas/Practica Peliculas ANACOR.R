# PRACTICA OPTATIVA ANACOR #

setwd("C:/Users/Edwin/Desktop/CUNEF/Master Data Science/Tecnicas de Agrupacion y reduccion de la dimension/Clase 4/Practica Anacor Optativa")
library(factoextra)
library(FactoMineR)

# Carga de la base de datos
peliculas<-matrix(c(70,45,30,0,35,0,45,30,80,5,0,0,30,20,10), nrow = 5, ncol = 3)
rownames(peliculas)<-c("Terror", "Comedia", "Drama", "Accion", "Otras")
colnames(peliculas)<-c("<25", "25-50", ">50")
peliculas

# Agrupación de filas por criterio: Comedia-Dramática, Terror-Acción y Otras
Comedia_Dramatica <- colSums(peliculas[c(2,3),])
Terror_Accion <- colSums(peliculas[c(1,4),])
Otras <- peliculas[-c(1:4),]
cross_gener <- rbind(Comedia_Dramatica, Terror_Accion, Otras)
cross_gener

# Al cruzar los géneros de las películas se solvento el incoveniente de requerir
# al menos 5 unidades muestrales por celda, dado que de manera individual 
# había películas con cero de audiencia.

library(tidyverse)
library(ggpubr)
ggballoonplot(cross_gener, fill = 'value', ggtheme = theme_gray()) +
  scale_fill_viridis_c(option = 'D') 
# En base al análisis de la gráfica en un nivel exploratio se observa 
# una mayor relación entro a edad de 25 a 50 años con el género de 
# Terror-Acción; mientras que, los menores a 25 años son propensos 
# a observar ambos géneros por igual. Por último, los mayores a 50
# años ostentan muy pocos valores.


# Análsis de correspondencias

cg.ca=CA(cross_gener, graph = F)
cg.ca 
summary.CA(cg.ca, nb.dec = 2, ncp = 2) 

# Al interior de los resultados del análisis de correspondencias, 
# se rechaza la prueba de independencia, aspecto que conlleva 
# que las variables se encuentran relacionas y se pueda continuar
# con el método restpectivo.

# En segunda instancia, se determina que la primera dimensión
# explica un 95.04% de la varianza; sin embargo, para poder realizar
# la visualización se converva la segunda dimensión.

# Al realizar un análisis por filas se destaca que la categoría otras,
# se encuentra correctamente representada por la primera dimensión,
# siendo su contribución de 83.99%. Por su parte, las comedias-dramáticas
# y el género de terror-acción contribuyen a expllicas en gran manera
# la segunda dimensión.

# En lo que corresponde al análisis por columnas, es evidente que la 
# inercia es superior en el grupo de 25 a 50 años; simultáneamente,
# contribuye en gran medida a explicar la primera dimensión. No obstante,
# la categoría que contribuye en la explicación de la segunda dimensión
# son las edades mayores a 50 años con un 78.70%.

plot(cg.ca, axes=c(1, 2), col.row = "blue", col.col ="red")

# Representación asimétrica de filas y columnas

fviz_ca_biplot(cg.ca, map ="colgreen",
               arrow = c(TRUE, FALSE))+
  ggtitle("Contribución de los géneros a las dimensiones")

# El gráfico de contribución por filas destaca la contibución de la
# categoría otras a la primera dimensión; en un sentido opuesto,
# la contribución de las comedias dramáticas es mayor en la segunda dimensión.

fviz_ca_biplot(cg.ca, map ="colgreen",
               arrow = c(FALSE, TRUE))+
  ggtitle("Contribución de los grupos de edad a las dimensiones")

# El gráfico de contribución por columas destaca la contibución de las
# edades entre 25 a 50 años es superior en la primera dimensión; miesntras,
# las dos categorías restantes evidencian una escasa contribución.

# Conclusiones:

# La validéz en la aplicación del análisis de correspondencias para el 
# relacionamiento entre grupos de edad y géneros de películas; fue comprobada
# al rechazar la prueba de independencia entre dichas variables.

# Para la información empleada el análisis de correspondencias, permite
# la reducción de las dimensiones a una sola (explicación del 95% de la varianza); 
# destacandose la eficiencia de emplear este método.

# El análisis gráfico, permite realizar una evidente abstracción determinando 
# a lados opuestos del plano las edades superiores a 50 años y el género de 
# Terror-Acción; lo que nos indica que las personas mayores tienen muy poco
# interés por ver este género de películas

# No obstante, la búsqueda de agrupación de las categorías para estos datos;
# nos se caracteriza de forma evidente de manera visual; aspecto que
# dificulta la segmentación por edad-género.
