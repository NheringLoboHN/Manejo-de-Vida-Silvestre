#Prueba Diagnóstica de Estadística
#Por: M.Sc. Nhering O. Lobo
#Clase: Manejo de Vida Silvestre
#Departamento de Ecología y Recursos Naturales
#Facultad de Ciencias - Escuela de Biología 
#Universidad Nacional Autónoma de Honduras

####################################################Categorías de Datos#####

#Un dato es cualquier pieza de información que se puede registrar, medir o analizar.En términos más generales, los datos son observaciones o hechos que representan características de objetos o eventos del mundo real. Los datos pueden estar relacionados con características cualitativas o cuantitativas. 

# 1) Datos Cualitativos (Categóricos)
#Nominales y Ordinales en R se manejan utilizando el tipo de dato factor.

#Nominales: Factores sin un orden específico. Ejemplo:
genero <- factor(c("masculino", "femenino", "femenino", "masculino"))

#Ordinales: Factores con un orden establecido. Ejemplo:
nivel_educacion <- factor(c("primaria", "secundaria", "universidad"), 
                          levels = c("primaria", "secundaria", "universidad"), ordered = TRUE)

# 2) Datos Cuantitativos
#Discretos: Los datos discretos en R normalmente se representan como enteros (integer). Ejemplo:
num_estudiantes <- c(25, 30, 28, 27)

#Continuos: Los datos continuos se representan como números de punto flotante (numeric). Ejemplo:
peso_aves <- c(0.5, 1.2, 0.9, 1.1)

# 3) Datos de Intervalo
#Los datos de intervalo, como temperaturas o fechas, se manejan normalmente como valores numeric o utilizando funciones especiales para fechas (Date o POSIXct). Ejemplo para fechas:

fechas <- as.Date(c("2023-01-01", "2024-01-01"))

# 4) Datos de Razón
#Los datos de razón, como el peso o la altura, también se tratan como numeric en R. 

altura_personas <- c(1.70, 1.75, 1.68, 1.80)

####Funciones clave en R para trabajar con estos datos####

# class(): Permite verificar el tipo de un objeto
class(genero)  # Verifica si es un factor

#as.factor(), as.numeric(), as.integer(): Funciones para convertir tipos de datos.
datos_categoricos <- as.factor(c("bajo", "medio", "alto"))

#Notas
# <- Este símbolo es el operador de asignación en R. Se usa para asignar valores a una variable o a un objeto. 
# c(): Esta es una función en R que significa combine o concatenate. Se usa para combinar múltiples valores en un vector, que es una estructura de datos básica en R.

################################################################################################################################################################################################################################################################################################################################

#Asignación de Variables
#En R, se utilizan variables para almacenar información o datos que luego pueden manipularse y analizarse.

# Asignar un valor a una variable
poblacion_venados <- 150  # Asigna el número 150 a la variable "poblacion_venados"

#El operador <- se usa comúnmente para asignar valores, aunque también se puede usar =.

#2. Vectores
#Los vectores son estructuras de datos que contienen una secuencia de elementos del mismo tipo (numéricos, de texto, etc.). Para crear un vector se utiliza la función c().

# Crear un vector numérico con los tamaños de diferentes poblaciones
poblaciones <- c(150, 200, 170, 180)

# Crear un vector de texto con los nombres de especies
especies <- c("Venado Cola Blanca", "Jaguar", "Tapir")

#Data Frames
#Un data frame es una estructura que permite almacenar datos en formato de tabla. Los data frames son útiles para organizar los datos de muestreos, observaciones de especies, entre otros.

# Crear un data frame con las especies y sus poblaciones
datos_especies <- data.frame(
  especie = c("Venado", "Jaguar", "Tapir"),
  poblacion = c(150, 25, 50)
)

##########################################################################################
#Lectura de Datos desde Archivos
#Es común tener datos en archivos CSV. Puedes cargar esos datos directamente en R con la función read.csv().

# Leer datos desde un archivo CSV
datos_muestreo <- read.csv("ruta/datos_muestreo.csv")

##########################################################################################

#Funciones Básicas de Resumen
#R tiene funciones incorporadas para realizar resúmenes básicos de datos, como promedios, máximos, mínimos, y más.

# Calcular el promedio de la población
promedio_poblacion <- mean(poblaciones)

# Encontrar el número mínimo y máximo de una población
min_poblacion <- min(poblaciones)
max_poblacion <- max(poblaciones)
min_poblacion
max_poblacion

#Filtrar Datos
#Para analizar subconjuntos de datos, puedes filtrar filas según ciertos criterios.

# Filtrar las especies con poblaciones mayores a 100
especies_mayores_100 <- datos_especies[datos_especies$poblacion > 100, ]
especies_mayores_100

#Visualización de Datos
#Puedes usar la biblioteca ggplot2 para crear gráficos que ayuden a visualizar los datos de forma clara y eficiente.

# Instalar y cargar la librería ggplot2
install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de barras con las poblaciones de especies
ggplot(datos_especies, aes(x = especie, y = poblacion)) +
  geom_bar(stat = "identity") +
  theme_minimal()

#Análisis Estadístico
#Si quieres realizar análisis estadísticos, como pruebas t o regresiones lineales, R tiene funciones integradas para ello.

# Realizar una prueba t para comparar poblaciones
t.test(poblacion ~ especie, data = datos_especies)

#######################################
# Filtrar dos especies
datos_filtrados <- subset(datos_especies, especie %in% c("Tapir", "Venado"))

table(datos_filtrados$especie)

datos_filtrados <- subset(datos_filtrados, !is.na(poblacion))

unique(datos_especies$especie)
summary(datos_filtrados)
#######################################

# Ajustar un modelo de regresión lineal
modelo <- lm(poblacion ~ especie, data = datos_especies)
summary(modelo)

#Manipulación de Datos con dplyr
#La biblioteca dplyr facilita la manipulación de grandes conjuntos de datos.

# Instalar y cargar la librería dplyr
install.packages("dplyr")
library(dplyr)

# Seleccionar columnas específicas
especies_seleccionadas <- datos_especies %>% select(especie, poblacion)

# Filtrar datos
especies_filtradas <- datos_especies %>% filter(poblacion > 100)
especies_filtradas

#Exportación de Resultados
#Después de realizar análisis o crear gráficos, puedes guardar los resultados o exportarlos.
# Guardar un gráfico como archivo PNG
ggsave("grafico_poblaciones.png")

# Exportar un data frame a un archivo CSV
write.csv(datos_especies, "resultados_especies.csv")


