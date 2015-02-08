## Reto_6_Producto_2: Clasificación de los distintas poblaciones de robledal 
## en función de las variables biofisicas estudiadas.

# Cargar los siguientes paquetes (se requieren para obtener este producto):
library (raster)
library (sp)
library (rgdal)
library (classInt)
library (RColorBrewer)

getwd () # Conocer el espacio de trabajo en el que estamos.

# Leer los datos de nieve.
robledal <- read.csv ("/home/mercesc/ecoinformatica_2014_2015/sesion_6/retos/producto_2/robles_ecoinfo.csv", header = TRUE, sep = ",", dec=".")
str (robledal) # Se visualiza la estructura de la tabla.

v_amb <- subset (robledal, select = -c (x,y)) # Extraer las variables de coordenada (x e y) y dejar solo las variables ambientales, que son las que se utilizarán para hacer la clasificación.
str (v_amb) # Mostrar la estructura de v_amb para ver que la función subset se haya efectuado correctamente.

n_centroides <- 3 # Variable que indica el numero óptimo de centroides. 
                  # Escojo 3, porque es el número de centroides que muestra una clasificación más clara del robledal. 
                  # También se puede ajustar este núero fijándonos en los valores de SS (sum of squares) que aparecen al hacer el cluster, 
                  # que van disminuyendo conforme aumenta el número de centroides (cuando la disminución de SS sea menos abrupta, el valor de centroides que le corresponda será teóricamente el más adecuado).

cluster <- kmeans (v_amb, n_centroides, iter.max=200) # Realizar el claster, con la función kmeans.
cluster # Se visualiza "claster" para ver el valor de SS, que nos interesa para poder seleccionar que numero de centroides es el optimo para este caso.

v_xy <- subset (robledal, select= c (x,y)) # Se crea una variable que nos muestre solo el valor de las coordenadas (x e y).
str(v_xy) # Mostrar la estructura de v_xy para ver que la función subset se haya efectuado correctamente.

v_xy<-cbind(v_xy, cluster$cluster) # Unir en columnas v_xy y la variable cluster que se encuentra dentro de cluster.
str(v_xy) # Mostrar la estructura de v_xy para ver que la función subset se haya efectuado correctamente.

colnames (v_xy)[3] <- "cluster" # Establecer la columna cluster de la variable v_xy en forma de matriz.

# Crear y pintar el mapa:
coordinates (v_xy) = ~x+y # Definir las coordenadas de los puntos.
proj4string (v_xy) = CRS ("+init=epsg:23030") # Definir el sistema de coordenadas WGS84.
plotclr <- rev (brewer.pal (n_centroides, "Spectral")) # Obtener "n_centroides" colores para una paleta de colores que se llama "Spectral", para cada cluster creado.
plot(v_xy, col=plotclr[v_xy$cluster], pch=20, cex = .5, main = "Mapa de clasificación del robledal") # Hacer el plot.

