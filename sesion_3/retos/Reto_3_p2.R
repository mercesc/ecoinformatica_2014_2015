## Reto_3_p2: Algoritmo que dado un umbral por el usuario,
## dados 10 numeros por el usuario,
## cuente cuantos de esos numeros supera el umbral indicado.

A <- scan (n=10) # Se listan los 10 valores que introduce el usuario.

umbral <- scan (n=1) # El umbral que tiene que poner el usuario.

contar <- 0 # Variable donde se van a ir contando los valores que superen el umbral.

for (valor in a){ # Para cada valor de "a", hacer lo siguiente:
  if (valor > umbral){ # Siempre que el valor de "a" sea mayor que el umbral:
    contar <- contar + 1 # Sumarle 1 a la variable "contar". Así se van contando cuantos números de los que da el usuario sobrepasan el umbral.
  }
} # Se cierra el bucle.

print (contar) # Muestra el resultado de "contar".

