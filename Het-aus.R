

install.packages("dismo")
install.packages("maptools")
install.packages("raster")
install.packages("sp")
install.packages("rgeos")
install.packages('rgeos', type="source")
install.packages('rgdal', type="source")
install.packages("rgeos")
install.packages("jsonlite")


library(dismo)
library(raster)
library(sp)
library(rgeos)

HetausGbif <- gbif("Heteromys","australis", geo = TRUE)


write.csv(HetausGbif, "Het-ausGbif.csv")

HetausGbif <- read.csv("Het-ausGbif.csv")

library(maptools)
data(wrld_simpl)

plot(wrld_simpl, xlim=c(-100,40), ylim=c(-60,40), axes=TRUE, col="light yellow")
box()

points(HetausGbif$lon, HetausGbif$lat, bg='orange', pch=21, cex=0.75)

dupl <- duplicated(HetausGbif[, c('lon', 'lat')])

sum(dupl)


HetausGbifCd <- HetausGbif[!dupl,]


lolNA <- which(is.na(HetausGbifCd$lon)&is.na(HetausGbifCd$lat))
HetausGbifCd <- HetausGbifCd[-lolNA,] ### aquí había un error, estabas quitando 
##los NA del archivo "HetausGbif" en lugar de "HetausGbifCd", que es el que ya tiene los datos sin duplicados.


#Verificar que los datos correspondan a los países 
csHeta <- HetausGbifCd[-lolNA,] 
csHeta$country <- droplevels(as.factor(csHeta$country), exclude = 0) 
coordinates(csHeta) <-  ~lon+lat
crs(csHeta) <- crs(wrld_simpl)
class(csHeta)
#Sobreponer los puntos de registro en el mapa
ovr <- over(csHeta, wrld_simpl)
head(ovr)
#NA no corresponden a ningún país #Datos del óceano 
cntr <- ovr$NAME
#which nos permite ver la ubicación de los puntos
i <- which(is.na(cntr))
length(i) #Cuantos datos sin país: 1
cntr <- ovr$NAME
#Definimos las ocurrencias que coinciden
j <- which(as.character(cntr)==as.character(csHeta$country))
#Vemos a que corresponden los datos que no coinciden
cbind.data.frame(cntr, csHeta$country)[-c(j,i),]
# Eliminemos los datos que caen en el mar (NA) y los que tienen 
#coordenadas erradas y veamos como quedan nuestros datos.
#removemos los datos que con ubicacion NA
HetamodF <- HetausGbifCd[-c(i, j[2]),]
write.csv(HetamodF, "Heteromys_ausGbif.csv")


#para leer los datos limpios y continuar con el ejercicios

HetamodF <- read.csv("Heteromys_ausGbif.csv")

## --EJERCICIO--
HetamodF<- read.csv("Heteromys_ausGbif.csv")
##si tenemos dos registros de ocurrencia cercanos,por debajo de
##200 metros (que es la resolucion de las variables explicativas), es
##necesario quedarnos con uno solo de estos registros. 
##ahora eliminaremos los registros que se encuentran en una misma celda
#3Obtenemos los datos del paquete dismo
files <- list.files(path=paste(system.file(package="dismo"), '/ex',
                               sep=''),  pattern='grd',  full.names=TRUE )
#Usamos el primer raster para obtener la mascara 
mask <- raster(files[1])
maskU <- mask
values(maskU) <- 1:ncell(mask)

#convertimos en un objeto tipo SpatialPointDataFrame
HetaRep <- HetamodF
coordinates(HetaRep) <- ~lon+lat
projection(HetaRep) <- CRS('+proj=longlat +datum=WGS84')

#Usamos extrac para obtener el valor de cada punto de ocurrencia
xp <- extract(mask, HetaRep)
length(xp) #la cantidad de puntos que tenemos
length(unique(xp)) #la cantidad de puntos con una ubicación mayor a 500m
#definimos la ubicación de los duplicados
HetaDes <- which(duplicated(xp))

#usamos este vector para eliminar duplicados
HetamodFD <- HetamodF[-HetaDes,]

#graficamos nuevamente
plot(wrld_simpl, xlim=c(-100,50), ylim=c(-50,30), axes=TRUE, col="light yellow")
box()
# incluimos los puntos
points(HetamodFD$lon, HetamodFD$lat, bg='orange', pch=21, cex=0.75)##ahora tenemos
##datos limpios y listos para ser usados en nuestro madelo.

##-Datos de ausencia y de fondo

# Seleccionamos 500 puntos aleatorios
#aseguramos tener los mismos datos aleatorios
set.seed(1963)
#Generamos los datos aleatorios
bg <- randomPoints(mask, 500 ) 

#generamos un cuadrante donde generar los datos aleatorios
e <- extent(-85, -70, -10, 5)
bg2 <- randomPoints(mask, 50, ext=e)

# set up the plotting area for two maps
par(mfrow=c(1,2), mar=c(3,3,1,1))
plot(!is.na(mask), legend=FALSE)
points(bg, cex=0.3, pch=21)
# now we repeat the sampling, but limit
# the area of sampling using a spatial extent
plot(!is.na(mask), legend=FALSE)
plot(e, add=TRUE, col='red')
points(bg2, cex=0.3, pch=21)
# Generamos círculos con un radio de 50 km
# usamos el objeto tipo SpatialPointDataFrame
x <- circles(HetaRep, d=50000, lonlat=TRUE)
## Lo convertimos en un polígono
pol <- polygons(x)
# Muestreamos aleatoriamente dentro del polígono
# extraemos 250 puntos
set.seed(34)
samp1 <- spsample(pol, 250, type='random', iter=25)

# Extraemos el valor para cada punto de la mascara
cells <- cellFromXY(mask, samp1)
length(cells) #tenemos 250 puntos los que generamos
#Eliminamos puntos repetidos
#Los puntos que tengan el mismo valor serán aquellos que 
#están dentro de una misma celda
cells <- unique(cells)
length(cells) #17 puntos estaban en una misma celda
#los eliminamos y nos quedan 233 puntos 

#obtenemos las coordenadas de esos puntos
xy <- xyFromCell(mask, cells) 

##Graficamos
plot(pol, axes=TRUE)
points(xy, cex=0.4, pch=3, col='blue')

