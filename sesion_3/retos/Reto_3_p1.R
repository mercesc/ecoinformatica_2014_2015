## Reto_3_p1: Algoritmo que multiplique 5 numeros indicados por el usuario.

A <- scan (n=5) # Listado de números introducidos por el usuario.

multi <- 1 # Variable que va a ir añadiendo las multiplicaciones.

for (valor in A){ # Para cada valor de "A", haz la operacion siguiente:
  multi <- multi * valor # "multi" por valor de "A" (se acumula en "multi" la multiplicación).
}

print (multi) # Muestra el resultado de multi (la multiplicación de los 5 números).
