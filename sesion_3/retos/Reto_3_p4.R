## Reto_3_p4: Dado el conjunto de datos ndvi.zip. 
## Cada imagen raster se tomó un día, a una hora y minutos concretos. 
## Queremos hacer un script que muestre la evolución del NDVI medio para las horas del día.

library(raster) # Cargar paquete raster.
library (sp) # Cargar paquete sp.
library (rgdal) # Cargar paquete rgdal.

horas <- c("12", "13", "14", "15") # Variable que contiene una cadena con las horas que nos interesan.

v0 <- c() # Vector vacío donde se acumularán los valores resultantes en cada vuelta del bucle.

for (valores in horas) { # Para los valores dentro de "horas", hacer lo siguiente:
  imagenes <- list.files (path="./ndvi", full.names = TRUE,pattern=paste("_", valores , "..\\.jpg\\.asc$", sep="")) # Abrir las imágenes que contengan ese valor.
  apilado <- stack (imagenes) # Apilar las imagenes correspondientes al valor de "horas".
  media <- mean (apilado) # Calcular el valor medio de cada píxel de las imágenes, generando una única imagen que contiene un valor medio para cada píxel.
  mediaunica <- cellStats (media, stat='mean') # Hacer la media de los valores de los píxeles de la imagen anterior, obteniendo un único valor.
  v0 <- rbind (v0, mediaunica) # Acumular los valores de "mediaunica" para cada valor de "horas" (cada vuelta en el bucle).
} # Se cierra el bucle.

v0 # Ver el contenido de v0 (para comprobar que obtenemos lo deseado).

# Por último, se realiza la gráfica para visualizar la evolución del NDVI a lo largo de las horas analizadas.
plot (horas, v0, type = "o", xlab="Horas", ylab="Valor NDVI medio", xlim = c(12,15), ylim = c(0.071, 0.081), main = "Evolución NDVI medio", col=3, pch=19) 

