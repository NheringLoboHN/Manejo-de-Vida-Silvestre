#Por: M.Sc. Nhering O. Lobo
#Clase: Manejo de Vida Silvestre
#Departamento de Ecología y Recursos Naturales
#Facultad de Ciencias - Escuela de Biología 
#Universidad Nacional Autónoma de Honduras
##################################################################################################################################################################

# Cargar librería ggplot2 para gráficos
library(ggplot2)

# Introducción del ejemplo
# En este ejemplo, vamos a utilizar el método Lincoln-Petersen para estimar la población de ratones comunes (Mus musculus) en Ciudad Universitaria. El estudio se realizó durante una semana utilizando trampas Havahart. Los ratones capturados fueron marcados y liberados.

# Datos de muestreo
# M = Número de ratones capturados y marcados en la primera captura
# C = Número total de ratones capturados en la segunda captura
# R = Número de ratones recapturados (es decir, los que ya estaban marcados)

# Ejemplo de datos obtenidos en el muestreo:
# Día 1: Se capturaron y marcaron 30 ratones.
# Día 7: Se capturaron 25 ratones, de los cuales 10 ya estaban marcados.

M <- 30  # Número de ratones capturados y marcados en la primera captura
C <- 25  # Número total de ratones capturados en la segunda captura (marcados y no marcados)
R <- 10  # Número de ratones recapturados que estaban marcados en la segunda captura

# Estimación del tamaño poblacional usando el método Lincoln-Petersen
# La fórmula es: N = (M * C) / R
N_est <- (M * C) / R

# Imprimir el tamaño estimado de la población
cat("El tamaño estimado de la población de ratones (Mus musculus) es:", N_est, "\n")

# Explicación:
# La fórmula del método Lincoln-Petersen utiliza el número de individuos marcados en la primera captura (M), el número total capturado en la segunda captura (C) y cuántos de esos ya estaban marcados (R).
# Esto nos da una estimación del tamaño total de la población (N).

# Para mejorar la precisión en estudios con muestras pequeñas, usamos el estimador de Chapman:
# Chapman es una modificación del método Lincoln-Petersen que evita sesgos cuando las muestras son pequeñas.

N_chapman <- ((M + 1) * (C + 1)) / (R + 1) - 1

# Imprimir el tamaño estimado de la población usando el estimador de Chapman
cat("El tamaño estimado de la población usando el estimador de Chapman es:", N_chapman, "\n")

# Visualización de cómo varía la estimación de la población con diferentes números de recaptura (R)

# Crear una secuencia de posibles valores de R (número de recapturados)
R_values <- seq(1, C, by = 1)  # R varía desde 1 hasta C (el total capturado en la segunda ronda)
R_values

# Calcular las estimaciones de la población para diferentes valores de R usando la fórmula de Lincoln-Petersen
N_est_values <- (M * C) / R_values
N_est_values

# Crear un dataframe con los resultados para graficar
df <- data.frame(R_values, N_est_values)
df

# Graficar el cambio en la estimación del tamaño poblacional en función de R
ggplot(df, aes(x = R_values, y = N_est_values)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Estimación del Tamaño Poblacional vs Número de Recapturados (R) - Lincoln-Petersen",
       x = "Número de Ratones Recapturados (R)",
       y = "Tamaño Estimado de la Población (N)") +
  theme_minimal()

# Ahora, incluimos también el estimador de Chapman para comparar

# Calcular las estimaciones usando el estimador de Chapman
N_chapman_values <- ((M + 1) * (C + 1)) / (R_values + 1) - 1
N_chapman_values

# Crear un dataframe con los resultados para Chapman
df_chapman <- data.frame(R_values, N_chapman_values)
df_chapman

# Crear gráfico comparativo entre Lincoln-Petersen y Chapman
ggplot() +
  geom_line(data = df, aes(x = R_values, y = N_est_values), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(data = df_chapman, aes(x = R_values, y = N_chapman_values), color = "green", size = 1.2, linetype = "dashed") +
  labs(title = "Comparación de Estimaciones: Lincoln-Petersen vs Chapman",
       x = "Número de Ratones Recapturados (R)",
       y = "Tamaño Estimado de la Población (N)") +
  theme_minimal() +
  geom_point(data = df, aes(x = R_values, y = N_est_values), color = "red", size = 2)
