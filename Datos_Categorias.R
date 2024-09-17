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


