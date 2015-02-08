## Reto_6_Producto_1: mapa que muestra la tendencia de la duración de la nieve y el valor de NDVI 
## para cada píxel en toda la serie temporal 2000-2012

# Cargar los siguientes paquetes (se requieren para obtener este producto):
library (Kendall)
library (wq)
library (plyr)
library (raster)
library (sp) 
library (rgdal)
library (classInt)
library (RColorBrewer)
 
### Análisis de tendencia de la duración de la nieve:

getwd () # Conocer el espacio de trabajo en el que estamos.

# Leer los datos de nieve.
nieve <- read.table ('/home/mercesc/ecoinformatica_2014_2015/sesion_6/retos/producto_1/nieve_robledal.csv', header = TRUE, sep=';')
str (nieve) # Explorar los datos.

nieve_t <- data.frame () # Crear un data.frame vacío.
nieve_t2 <- data.frame (nie_malla_modi_id=NA, tau_nieve=NA, pvalor_nieve=NA) # Crear un data.frame que contenga las 3 variables indicadas, donde se irán acumulando los resultados obtenidos en el bucle.
pixels <- unique (nieve$nie_malla_modi_id) # Seleccionar la variable "nie_malla_modi_id" y darle el nombre de pixels.

# Realizar un bucle para calcular los tau y p-values de cada pixel.
for (valores in pixels){ # Para cada valor de "pixels", hacer lo siguiente:
  cod2 <- nieve [nieve$nie_malla_modi_id==valores,] # Asociar cada valor de "pixels" a la variable "nie_malla_modi_id" del data.frame.
  Kendall <- MannKendall (cod2$scd) # Hacer el test de Mann-Kendall para ver la tendencia de cada valor de "pixels".
  nieve_t2$nie_malla_modi_id <- valores # Asociar cada valor de "pixels" con el data.frame "nieve_t2".
  nieve_t2$tau_nieve <- Kendall[[1]][1] # Asociar cada valor de tau con el data.frame "nieve_t2".
  nieve_t2$pvalor_nieve <- Kendall[[2]][1] # Asociar cada p-valor con el data.frame "nieve_t2".
  nieve_t <- rbind (nieve_t, nieve_t2) # Unir los datos de "nieve_t2" con el data.frame vacío.
} # Cerrar bucle.

nieve <- nieve [,c(2,10,11)] # Generar la tabla "nieve" con las columnas que contienen los datos de interés.
nieve_unico <- unique (nieve) # Generar una tabla que seleccione un único valor de los píxeles.
nieve_final <- join (nieve_unico, nieve_t, by="nie_malla_modi_id") # Unir los data.frame.
head (nieve_final) # Visualizar la cabecera del data.frame "nieve_final".

# Crear y pintar el mapa:
coordinates (nieve_final) = ~lng+lat # Definir las coordenadas de los puntos.
proj4string (nieve_final) = CRS ("+init=epsg:4326") # Definir el sistema de coordenadas WGS84.
clases1 <- classIntervals (nieve_final$tau_nieve, n = 5) # Separar los valores de tau en 5 clases.
plotclr1 <- rev (brewer.pal (5, "Spectral")) # Obtener cinco colores para una paleta de colores que se llama "Spectral".
colcode <- findColours (clases1, plotclr1) # Asociar los valores de tau a su valor correspondiente. 
plot (nieve_final, col=colcode, pch=20, cex = .5, main = "Tendencia de la duración de la nieve") # Hacer el plot.
legend ("topright", legend=names (attr (colcode, "table")), fill=attr (colcode, "palette"), bty="n") # Mostramos la leyenda.


### Análisis de tendencia de la duración del NDVI:

getwd () # Conocer el espacio de trabajo en el que estamos.

# Leer los datos del NDVI.
ndvi <- read.table('/home/mercesc/ecoinformatica_2014_2015/sesion_6/retos/producto_1/ndvi_robledal.csv', header = TRUE, sep=';')
str (ndvi) # Explorar los datos.

ndvi_t <- data.frame ()  # Crear un data.frame vacío.
ndvi_t2 <- data.frame (iv_malla_modi_id=NA, tau_ndvi=NA, pvalor_ndvi=NA) # Crear un data.frame que contenga las 3 variables indicadas, donde se irán acumulando los resultados obtenidos en el bucle.
pixels <- unique (ndvi$iv_malla_modi_id)  # Seleccionar la variable "iv_malla_modi_id" y darle el nombre de pixels.

# Realizar un bucle para calcular los tau y p-values de cada pixel.
for (valores in pixels){ # Para cada valor de "pixels", hacer lo siguiente:
  cod <- ndvi [ndvi$iv_malla_modi_id==valores,] # Asociar cada valor de "pixels" a la variable "iv_malla_modi_id" del data.frame.
  Kendall <- MannKendall (cod$ndvi_i) # Hacer el test de Mann-Kendall para ver la tendencia de cada valor de "pixels".
  ndvi_t2$iv_malla_modi_id <- valores # Asociar cada valor de "pixels" con el data.frame "ndvi_t2".
  ndvi_t2$tau_ndvi <- Kendall[[1]][1] # Asociar cada valor de tau con el data.frame "ndvi_t2".
  ndvi_t2$pvalor_ndvi <- Kendall[[2]][1] # Asociar cada p-valor con el data.frame "ndvi_t2".
  ndvi_t <- rbind (ndvi_t, ndvi_t2) # Unir los datos de "ndvi_t2" con el data.frame vacío.
} # Cerrar bucle.

ndvi <- ndvi [,c(1,4,5)]  # Generar la tabla "ndvi" con las columnas que contienen los datos de interés.
ndvi_unico <- unique (ndvi) # Generar una tabla que seleccione un único valor de los píxeles.
ndvi_final <- join (ndvi_unico, ndvi_t, by="iv_malla_modi_id") # Unir los data.frame.
head (ndvi_final) # Visualizar la cabecera del data.frame "ndvi_final".

# Crear y pintar el mapa:
coordinates (ndvi_final) = ~lng+lat # Definir las coordenadas de los puntos.
proj4string (ndvi_final) = CRS ("+init=epsg:4326") # Definir el sistema de coordenadas WGS84.
clases <- classIntervals (ndvi_final$tau_ndvi, n = 5) # Separar los valores de tau en 5 clases.
plotclr <- rev (brewer.pal (5, "Spectral")) # Obtener cinco colores para una paleta de colores que se llama "Spectral".
colcode <- findColours (clases, plotclr) # Asociar los valores de tau a su valor correspondiente.
plot (ndvi_final, col=colcode, pch=20, cex = .5, main = "Tendencia del NDVI") # Hacer el plot.
legend ("topright", legend=names (attr (colcode, "table")), fill=attr (colcode, "palette"), bty="n") # Mostramos la leyenda.

