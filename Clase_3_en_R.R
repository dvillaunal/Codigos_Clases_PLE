# ==========================
# Instalación de paquete:
# ==========================

install.packages("catdata") # Forma con código 
                            # Es preferible usar la interfase del RStudio


# ==========================
# Cargando paquetes:
# ==========================

require(catdata) # Forma 1
library(catdata) # Forma 2

# ====================================
# Proceso inverso a cargar paquetes:
# ====================================

detach("package:catdata", unload = TRUE) # Es preferible usar la interfase del RStudio

# ========================================
# Pidiendo ayuda de paquetes y funciones:
# ========================================

help(catdata) # Forma 1
?catdata      # Forma 2
??catdata     # Forma 3 - Cuando no se dónde está el paquete o la función

# ========================================
# Creando data frames:
# ========================================

edad <- c(31, 23, 24, 54, 19)
nombre <- c("Pedro", "Luis", "Carla", "Ana", "María")
genero <- c("M", "M", "F", "F", "F")
puntaje <- c(2.3, 4.2, 2.1, 4.9, 3.1)
df <- data.frame(nombre, edad, genero, puntaje)
df

# Forma equivocada:
ma <- cbind(nombre, edad, genero, puntaje)
ma

# Estructura del data frame:
str(df)

# ==============================
# Tipos de datos o variables: 
# ==============================
a1 <- 3
class(a1)

a2 <- "3"
class(a2)

a3 <- "Pedro"
class(a3)

b1 <- c(3, 2, 6, 1)
class(b1)

b2 <- c("3", "2", "6", "1")
class(b2)

b3 <- c("Pedro", "Juan", "Luis")
class(b3)

b4 <- c("3", 2, 6, 1)
class(b4)

# Otra forma:

a1 <- 3
typeof(a1)

a2 <- "3"
typeof(a2)

a3 <- "Pedro"
typeof(a3)

b1 <- c(3, 2, 6, 1)
typeof(b1)

b2 <- c("3", "2", "6", "1")
typeof(b2)

b3 <- c("Pedro", "Juan", "Luis")
typeof(b3)

b4 <- c("3", 2, 6, 1)
typeof(b4)

# ==================================
# Tipo factor versus tipo caracter:
# ==================================

c1 <- factor(c(4,4,3,1,5,2,3))
class(c1)

c2 <- c("4","4","3","1","5","2","3")
class(c2)

# ==================================
# Pasando de un tipo a otro:
# ==================================

# Numérico a caracter:
b1 <- c(3, 2, 6, 1)
class(b1)

b2 <- as.character(b1)
class(b2)
b2

# Caracter a Numérico:
c1 <- c("3", "2", "6", "1")
class(c1)

c2 <- as.numeric(c1)
class(c2)
c2

# Factor a caracter:
d1 <- factor(c(5, 3, 1, 2, 3))
class(d1)

d2 <- as.character(d1)
class(d2)
d2

# Caracter a factor:
e1 <- c("5", "3", "1", "2", "3")
class(e1)

e2 <- as.factor(e1)
class(e2)
e2

# =========================
# Secuencias en R:
# =========================

# Secuencia de enteros:
a1 <- 1:11
a1

# Secuencia de valores entre dos con un valor de salto:
a2 <- seq(1, 5, 0.5)
a2

# Repetición de valores o secuencia:
a3 <- rep( c(1,4,2), 4)
a3

# =========================
# Funciones estadísticas:
# =========================

# Media muestral o promedio:
v1 <- c(3, 4, 1, 8, 10)
mean(v1)

# Falla por valor faltante:
v2 <- c(3, NA, 1, 8, 10)
mean(v2)

# Solución al problema anterior:
mean(v2, na.rm=TRUE)

# Varianza muestral:
v1 <- c(3, 4, 1, 8, 10)
var(v1)

# Desviación estándar muestral:
v1 <- c(3, 4, 1, 8, 10)
sd(v1)

# Mínimo:
v1 <- c(3, 4, 1, 8, 10)
min(v1)

# Máximo:
v1 <- c(3, 4, 1, 8, 10)
max(v1)

# Mediana:
v1 <- c(3, 4, 1, 8, 10)
median(v1)

v2 <- c(3, 4, 1, 8, 10, 13)
median(v2)

# Cuantiles:
v3 <- c(15, 13, 10, 1, 3, 4, 7, 6, 17, 8)
quantile(v3, probs=c(0.25, 0.50, 0.75))

# Resumen estadístico básico:
v3 <- c(15, 13, 10, 1, 3, 4, 7, 6, 17, 8)
summary(v3)

# Tabla de frecuencias para variables tipo caracter:
v4 <- c("a", "b", "a", "b", "c", "c", "a")
table(v4)
summary(v4)

# Tabla de frecuencias para variables tipo factor:
v5 <- as.factor(v4)
table(v5)
summary(v5)

