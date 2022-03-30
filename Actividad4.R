#-------Codigo Clase 4-------
data()

require(datasets)
require(carData)
carros <- datasets::cars
dim(carros)
head(carros)
pairs(carros)
tail(carros)
xtabs(~dist,carros)

muestra <- data.frame(table(as.factor(sample(1:5,1000,replace = T))))

table(muestra)
xtabs(muestra)

prop.table(muestra)

#-------Actividad N°4-------

#----Dataset Heart----

#install.packages("catdata")
require(catdata)

# NO funciono con el paquete, mejor me redirigí
#a la ruta donde se encuntran los datasets
load("R/win-library/4.1/catdata/data/heart.rda")
heart <- as.data.frame(heart)

# Una muestra retrospectiva de varones en una región de
#alto riesgo de enfermedades cardíacas del Cabo Occidental, Sudáfrica.
# 
# Un conjunto de datos con 462 observaciones sobre 8 variables y 2 respuesta binaria.
# contiene 10 columnas de las siguientes variables:
# sbp (presión arterial sistólica) mmHg (milimiteros de mercurio)
# tabaco (tabaco acumulado) posiblemente en Gramos;
  # ldl (colesterol de lipoproteínas de baja densidad) miligramos por decilitro; 
# adiposidad (porcentaje %);
# famhist (antecedentes familiares de enfermedades cardíacas) (SI = 1, NO = 0);
# typea (comportamiento tipo A);
# obesidad; 
# alcohol (consumo actual de alcohol);
# edad (edad de inicio)
# y: respuesta, enfermedad coronaria

str(heart)
heart$y <- as.factor(heart$y)
heart$famhist <- as.factor(heart$famhist)


#----DataSets Birth----

load("R/win-library/4.1/catdata/data/birth.rda")


str(birth)

# ÍndiceMadre
# Variable ID
# 
# Sexo
# Sexo del niño: hombre = 1, mujer = 2
# 
# Peso
# Peso del niño al nacer en gramos
# 
# Altura
# Altura del niño al nacer en centímetros
# 
# Cabeza
# Perímetro cefálico del niño al nacer en centímetros
# 
# Mes
# Mes de nacimiento del 1 al 12
# 
# Año
# Año de nacimiento
# 
# País
# País de nacimiento: Francia (FR), Bélgica (BE), Suiza (CH), Canadá (CA), Gran Bretaña (GB), Alemania (DE), España (ES), Estados Unidos (US)
# 
# Término
# Duración del embarazo en semanas desde la última menstruación
# 
# EdadMadre
# Edad de la madre en el día del nacimiento
# 
# Anterior
# Número de embarazos anteriores
# 
# PesoAntes
# Peso de la madre antes del embarazo
# 
# AlturaMadre
# Altura de la madre en centímetros
# 
# PesoFinal
# Peso de la madre después del embarazo
# 
# Gemelos
# ¿El embarazo fue un parto múltiple? no = 0, sí = 1
# 
# Intensivos
# Días que el niño pasó en la unidad de cuidados intensivos
# 
# Cesárea
# ¿Ha nacido el niño por cesárea? no = 0, sí = 1
# 
# Planificada
# ¿La cesárea ha sido planificada? no = 0, sí = 1
# 
# Episiotomía
# ¿Se ha realizado una episiotomía? no = 0, sí = 1
# 
# Desgarro
# ¿Ha aparecido un desgarro perineal? no = 0, sí = 1
# 
# Operativo
# ¿Se ha utilizado una ayuda operativa como fórceps de parto o vakuum? no = 0, sí = 1
# 
# Inducido
# ¿Se ha inducido el parto artificialmente? no = 0, sí = 1
# 
# Membranas
# ¿Se han roto las membranas antes del comienzo de la agonía? no = 0, sí = 1
# 
# Reposo
# ¿Se ha ordenado a la madre un reposo estricto durante al menos un mes durante el embarazo? no = 0, sí = 1
# 
# Presentación
# ¿Presentación del niño antes del parto? presentación cefálica = 1, presentación pélvica = 2, otra presentación (por ejemplo, cruzada) = 3

prueba <- data.frame(table(birth$IndexMother))
rm("prueba")

