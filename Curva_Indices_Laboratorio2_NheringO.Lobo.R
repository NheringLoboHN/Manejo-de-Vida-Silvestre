#Curva de acumulación de especies e Índices de Biodiversidad
#Por: M.Sc. Nhering O. Lobo
#Clase: Manejo de Vida Silvestre
#Departamento de Ecología y Recursos Naturales
#Facultad de Ciencias - Escuela de Biología 
#Universidad Nacional Autónoma de Honduras

#############################################################################################################################################################################################################################################

# Cargar los paquetes necesarios
library(ggplot2)
library(vegan)
library(dplyr)

# Cargar los datos
data <- read.delim("Curva1.txt", header = TRUE, sep = "\t", row.names = 1)

# 1. Número total de observaciones
total_observaciones <- nrow(data)
print(paste("Número total de observaciones:", total_observaciones))

# 2. Número de sitios de muestreo únicos
sitios_unicos <- length(unique(data$Sitio))
print(paste("Número de sitios de muestreo:", sitios_unicos))

# Graficar número de observaciones por sitio
# Contar el número de observaciones por Sitio
data_observaciones <- data %>%
  group_by(Sitio) %>%
  summarise(Num_observaciones = n())

# Graficar número de observaciones por sitio ordenado de mayor a menor
ggplot(data_observaciones, aes(x = reorder(Sitio, -Num_observaciones), y = Num_observaciones)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Número de observaciones por Sitio de Muestreo", 
       x = "Sitio", 
       y = "Número de observaciones") +
  theme_minimal()

# 3. Número de especies reportadas
especies_reportadas <- data %>% 
  group_by(Especie) %>% 
  summarize(Abundancia_total = sum(Abundancia, na.rm = TRUE)) %>% 
  arrange(desc(Abundancia_total))

print(paste("Número total de especies:", nrow(especies_reportadas)))

# Gráfico de barras ordenado de mayor a menor
ggplot(especies_reportadas, aes(x = reorder(Especie, -Abundancia_total), y = Abundancia_total)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = Abundancia_total), vjust = -0.3, size = 3) +
  labs(title = "Abundancia de aves - UNAH-CU", x = "Especies", y = "Abundancia") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Curva de acumulación de especies
comunidad <- table(data$Sitio, data$Especie)
curva <- specaccum(comunidad, method = "random")

# Graficar la curva de acumulación
plot(curva, xlab = "Número de Sitios", ylab = "Riqueza de Especies", 
     main = "Curva de Acumulación de Especies - UNAH-CU", col = "blue", lwd = 2, ci.type = "poly", ci.lty = 0, ci.col = "lightblue")

# Explicación: La curva de acumulación muestra el número acumulado de especies conforme se van añadiendo más sitios de muestreo. 
# Un aplanamiento de la curva indica que se han capturado la mayoría de las especies en esos sitios.






##################################################################################################################### Indices de Biodiversidad ############################################################################

# Crear matriz de comunidad para el análisis de diversidad
comunidad_matrix <- as.matrix(table(data$Sitio, data$Especie))

# Índice de Simpson (D)
simpson_index <- diversity(comunidad_matrix, index = "simpson")
print(paste("Índice de Dominancia de Simpson (D):", mean(simpson_index)))

# Índice de Shannon-Wiener (H')
shannon_index <- diversity(comunidad_matrix, index = "shannon")
print(paste("Índice de Shannon-Wiener (H'):", mean(shannon_index)))

# Índice de Margalef
margalef_index <- (specnumber(comunidad_matrix) - 1) / log(rowSums(comunidad_matrix))
print(paste("Índice de Margalef:", mean(margalef_index)))

# Índice de Pielou (J)
pielou_index <- shannon_index / log(specnumber(comunidad_matrix))
print(paste("Índice de Equidad de Pielou (J):", mean(pielou_index)))

# Índice de Berger-Parker
berger_parker_index <- apply(comunidad_matrix, 1, function(x) max(x) / sum(x))
print(paste("Índice de Berger-Parker:", mean(berger_parker_index)))

# Índice de Equidad de Camargo
camargo_index <- apply(comunidad_matrix, 1, function(x) 1 - sum(abs(outer(x, x, "-"))) / (2 * sum(x) * (sum(x) - 1)))
print(paste("Índice de Equidad de Camargo:", mean(camargo_index)))


################################################################################## Gráfico combinado con diferentes índices de diversidad ###########################################################################

par(mfrow = c(2, 2)) # Crear una ventana con 2 filas y 2 columnas

# Índice de Shannon-Wiener
hist(shannon_index, main = "Índice de Shannon-Wiener", col = "coral", xlab = "Valor de Shannon-Wiener")

# Índice de Simpson
hist(simpson_index, main = "Índice de Simpson", col = "lightblue", xlab = "Valor de Simpson")

# Índice de Margalef
hist(margalef_index, main = "Índice de Margalef", col = "lightgreen", xlab = "Valor de Margalef")

# Índice de Berger-Parker
hist(berger_parker_index, main = "Índice de Berger-Parker", col = "lightpink", xlab = "Valor de Berger-Parker")

# Ajustar la ventana gráfica a una sola figura
par(mfrow = c(1, 1))