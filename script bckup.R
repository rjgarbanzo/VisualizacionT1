datos <- read.csv("temperaturasEuropa.csv", sep = ";", header = T, row.names = 1)
str(datos)

library(FactoMineR)

# Para el acp se omite la variable X que contiene los nombres

resultado_acp <- PCA(datos[,1:12], graph = F)

# resultado_acp$ind$coord 

# En este ruta se almacena la tabla con las coordenadas para los individuos

# agregamos a la tabla los nombres y las coordenadas de las primeras 2 componentes

datos_grafico <- data.frame(resultado_acp$ind$coord[,1:2])

# resultado_acp$ind$cos2 almacena el cos2 de cada uno de las componentes

# para tener el cos2 total sumamos los cos2 de las primeras dos componentes

datos_grafico <- cbind(datos_grafico, Cos2 = resultado_acp$ind$cos2[,1] + resultado_acp$ind$cos2[,2])

datos_grafico

# resultados_acp$eig contiene los valores propios de cada componente

# podemos saber la inercia explicada de cada componentes diviviendo su valor propio enter 

# la cantidad de componentes

datos_grafico <- cbind(datos_grafico, 
                       Iner.x = resultado_acp$eig[1,1]/nrow(resultado_acp$eig), 
                       Iner.y = resultado_acp$eig[2,1]/nrow(resultado_acp$eig))



# Ahora tenemos los datos necesarios para poder hacer el gráfico


head(datos_grafico)
library(ggplot2)


ggplot(datos_grafico, aes(x=datos_grafico$Dim.1, y=datos_grafico$Dim.2)) +
  geom_point(aes(color = ifelse(datos_grafico$Cos2 > 0.5, "black", "red")))+
  labs (y = datos_grafico$Iner.y,
        x = datos_grafico$Iner.x)+
  geom_text(mapping = aes(label = (row.names(datos_grafico))))+
  scale_color_identity()


datos_grafico$Cos2





########################################################################################

# Ejercicio 5

library(forcats)
datos <- read.csv("cost-of-living.csv", sep = ",", header = T)
costoVida <- as.data.frame(colSums(datos[2:55]))
colnames(costoVida) <- "Monto"


ggplot(data = costoVida,
       mapping = aes(
         x = fct_reorder(rownames(costoVida), costoVida$Monto),
         y = costoVida$Monto)) +
  geom_col(fill = "#07689f") +
  coord_flip() +
  labs(title = "Costo de vida por país",
       x = "Costo de Vida",
       y = "Ciudades más caras para vivir" )
  



############
library(plyr)
library(dplyr)
library(doBy)
str(datos)

# datos <- read.csv("players_20.csv", sep = ",", header = T)
# datos <- select(datos,5, 10)
# dtMean <- aggregate(datos[,1], list(datos$club), mean)


datos <- read.csv("players_20.csv", sep = ",", header = T)
datos <- select(datos, 5, 10, 13)
suppressWarnings(dtMean <- aggregate(datos[,1:3], list(datos$club), mean))


DatosMean <- dtMean[order(dtMean$value_eur),]
DatosMean <- subset(DatosMean, DatosMean$value_eur > 14953667)


ggplot(data = DatosMean,
       mapping = aes(
         x = DatosMean$age,
         y = DatosMean$value_eur)) +
  geom_point() +
  labs(title = "Costo de vida por país",
       x = "Promedio edad de Jugadores",
       y = "Promedio valor de jugadores" )+
  geom_text (mapping = aes(label=(DatosMean$Group.1)), vjust = -1)



  ##########

datos <- read.csv("AppleStore.csv", sep = ",", header = T)
datos <- select(datos, 6,9,13)
datos <- subset(datos, datos$price < 50)



ggplot(data = datos,
       mapping = aes(x = datos$price,y = datos$user_rating , color = datos$prime_genre)) +
  geom_point() +
  scale_x_continuous(breaks = c(-10,0,10,20,30,40,50))

+
  labs(title = "Costo de vida por país",
       x = "Promedio edad de Jugadores",
       y = "Promedio valor de jugadores" )+
  geom_text (mapping = aes(label=(DatosMean$Group.1)), vjust = -1)
