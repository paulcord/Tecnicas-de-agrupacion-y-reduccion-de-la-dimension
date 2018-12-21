### Análisis rendimiento de los bonos norteamericanos ###

# Delimitamos el WD
setwd("C:/Users/Edwin/Desktop/CUNEF/Master Data Science/Tecnicas de Agrupacion y reduccion de la dimension/Datos")

# Cargamos base de datos
bd <- read.csv("ACPTIUSD.csv", header = T, sep = ";", dec = ".")

# Identificar la estructura y dimensión de los datos
str(bd)

# Visualizamos los primeros y últimos datos
head(bd, 10)
tail(bd, 10)

## ANÁLISIS EXPLORATORIO
library(tidyverse)

# Identificación de datos NA
sum(is.na.data.frame(bd)==T) # 195 valores NA
sum(is.na(bd$DEPO.1M)==T) # Todos los NA son de la 1era variable

# Análisis del promedio
bd %>%
  summarise(
    D.1M=  round(mean(DEPO.1M, na.rm = T),2),
    D.3M=  round(mean(DEPO.3M),2),
    D.6M=  round(mean(DEPO.6M),2),
    D.12M= round(mean(DEPO.12M),2),
    D.2Y=  round(mean(IRS.2Y),2),
    D.3Y=  round(mean(IRS.3Y),2),
    D.4Y=  round(mean(IRS.4Y),2),
    D.5Y=  round(mean(IRS.5Y),2),
    D.7Y=  round(mean(IRS.7Y),2),
    D.10Y= round(mean(IRS.10Y),2)
  )

# Rangos de la tasa de interés
min(bd$DEPO.1M, na.rm = T) # Mínimo a 1 mes
max(bd$IRS.10Y) # Máximo a 10 años

# Histograma de densidad según variables
library("reshape2")

nbd <- melt(bd)
nbd1 <- filter(nbd, variable==c('DEPO.1M','DEPO.12M','IRS.5Y','IRS.10Y'))
ggplot(nbd1, aes(x=value, fill=variable), xlab('a')) +
  geom_histogram(aes(y = ..density..), na.rm = F, binwidth=1) +
  geom_density() +
  facet_grid(variable~.) + 
  labs(x='Tipo de Interés', y='Densidad') +
  ggtitle('Histograma de densidad según maduración')

# Curva de redimiento del los bonos
temporalidad <- c(1/12,3/12,6/12,1,2,3,4,5)
g <- gray.colors(6,start=0.001,end=0.75,gamma=2.2)
plot(temporalidad,
     bd[783,2:9], #no se incluyen NA
     type="l",
     xlab="Temporalidad(anual)",
     ylab="Tipo de interés(%)",
     lwd=3.5,
     ylim=c(5.3,7.1),
     col="darkred")
points(temporalidad,
       as.numeric(bd[783,2:9]),
       col="darkred")
for ( i in 1:6 ) {
  lines(temporalidad,
        as.numeric(bd[783-i*30,2:9]),
        type="l",
        col=g[i])
}

## ANÁLISIS COMPONENTES PRINCIPALES
  
# Carga de las librerías
library(factoextra)
library(FactoMineR)

## Análisis de Componentes Principales (ACP)

# Imputamos los valores NA en la variable DEPO.1M
library("missMDA")
nb <- estim_ncpPCA(bd[,-1],ncp.max=10) 
nb #Para el siguiente paso el número de componentes debe ser igual a 7
bdm <- as.data.frame(imputePCA(bd[,-1],ncp=7))
#Base de datos sin NA
bd1 <- bdm[,1:10]
names(bd1) <- sub(c("completeObs."), "", names(bd1))

#ACP- Observaciones y variables activas
ACP <- PCA(bd1[1:949,1:9], graph=T)  
ACP

#ACP- Observaciones y variables suplementarias
ACPs <- PCA(bd1, ind.sup = 950:978, 
            quanti.sup = 10, graph=FALSE)

# Predección para la variable cuantitativa IRS.10Y
ACPs$quanti.sup

# Gráfico de posición de los variable sumplementaria
fviz_pca_var(ACPs, alpha.var = 0.4)

## Aplicación de pruebas - Justificación uso de ACP
mcorr<-cor(na.omit(bd[1:945,2:10]))

#Matriz de correlación
mcorr  
#Determinante de la matriz de correlación
det(mcorr)  
#Test de esfericidad de Bartlett
library(psych)
cortest.bartlett(mcorr, n=nrow(bd))
#Test de Kaiser-Meyer-Olkin 
KMO(mcorr)

# Gráfico de sedimentación
fviz_eig(ACP, addlabels=T, 
         hjust=0.5, 
         barfill = 'gray',
         main=NULL)+
         labs(x="Dimensiones", 
         y="%Varianza explicada",
         title = "Gráfico Sedimentación") +
         ylim(0, 90)+
         theme_minimal()

# Rotación VARIMAX
fa.parallel(bd1[1:945,1:9])
varimax1 <- factanal(bd1[1:945,1:9], factors=2, rotation="varimax")
varimax1
