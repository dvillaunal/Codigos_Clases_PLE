#-------Actividad N°4-------

#----Dataset Heart----

#install.packages("catdata")
require(catdata)
library(tidyverse)
library(magrittr)
data("heart")
# NO funciono con el paquete, mejor me redirigí
#a la ruta donde se encuntran los datasets
heart <- as.data.frame(heart)

# Una muestra retrospectiva de varones en una región de
#alto riesgo de enfermedades cardíacas del Cabo Occidental, Sudáfrica.
# 
# Un conjunto de datos con 462 observaciones sobre 8 variables y 2 de respuesta binaria.
# contiene 10 columnas de las siguientes variables:

# y: respuesta, enfermedad coronaria (1:SI, 0:NO)

# sbp (presión arterial sistólica) mmHg (milimiteros de mercurio)

# tobaco: (tabaco acumulado) en Gramos;

# ldl (colesterol de lipoproteínas de baja densidad) miligramos por decilitro; 

# adiposidad (porcentaje %);

# famhist (antecedentes familiares de enfermedades cardíacas) (SI = 1, NO = 0);

# typea (comportamiento tipo A, "test");

# obesidad (IMC, Indice del Masa Corporal); 

# alcohol (consumo actual de alcohol en ml milimetros);

# age: edad (edad de inicio)

dim(heart)
str(heart)

heart$y <- as.factor(heart$y)
names(heart)[1] <- "coronary"
heart$famhist <- as.factor(heart$famhist)

head(heart)
tail(heart)

summary(heart)

heart %>%  ggplot(aes(x=adiposity, y=ldl, color= coronary))+
  geom_point()+
  labs(x="Porcentaje (%) de Adiposidad", y="Colesterol tipo \"ldl\" (mg/dl)",
       title = "Relación entre Colesterol y el porcentaje de Adiposidad")+
  scale_color_discrete("Enfermedad\nCoronaria", labels=c("No", "Si"))

heart$obesity %<>% cut(.,breaks= c(0,18.5,24.9,29.9,Inf),
                      labels=c("Bajo peso", "Normal", "Sobrepeso", "Obesidad"))


heart %>% ggplot(aes(x= obesity, y=sbp, fill=obesity))+geom_violin()+
  theme(legend.position = "none")+
  labs(x="IMC por niveles de peso",
       y="Presión arterial sistólica (mmHg)",
       title = "Presión arterial sistólica por cada nivel del peso del IMC")

#----DataSets Birth----
# datos de "birth" contienen información sobre el nacimiento
# y el embarazo de 775 niños que nacieron vivos en el periodo
# comprendido entre 1990 y 2004.
# Los datos se recogieron de usuarios de Internet reclutados
# en sitios web francófonos sobre el embarazo y el nacimiento

data("birth")


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
# Sudáfrica (AFRIQUESUD), Finlandia (FI), Grecia (GR), Irlanda (IR), Italia (IT), Lituania (LI), Luxemburgo (LU), Macao (MO), Holanda (NL), Omán (OMAN)
# Singapur (SINGAPURE), Checoslovaca (TCH)
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

str(birth)
dim(birth)

birth$Sex %<>% as.factor() %>% factor(.,labels = c("hombre", "mujer"))
birth$Month %<>% as.factor()
birth$Year %<>% as.factor()
birth$Twins %<>% as.factor()
birth$Cesarean %<>% as.factor()
birth$Planned %<>% as.factor()
birth$Episiotomy %<>% as.factor()
birth$Tear %<>% as.factor()
birth$Operative %<>% as.factor()
birth$Induced %<>% as.factor()
birth$Membranes %<>% as.factor()
birth$Rest %<>% as.factor()
birth$Presentation %<>% as.factor()

head(birth)
tail(birth)
summary(birth)

birth %>% ggplot()+
  geom_density(aes(x=WeightBefore,fill="Antes"), alpha=0.4)+
  geom_density(aes(x=WeightEnd,fill="Después"), alpha=0.4)+
  facet_grid(Sex~.)+
  labs(x="Peso de la madre en Kg", y = "Densidad",
       title = "Peso de las madres, antes y después del Parto\nfacetado por genero")+
  theme(legend.title = element_blank())

birth$AgeMother %<>%  cut(.,breaks=c(16,25,35,42))

birth$Term %<>% cut(.,breaks=c(28,37,42), labels=c("pretermino", "termino"))



# Determinado por el peso
birth %<>% mutate_at(.,c("Term"),~replace(.,is.na(.),"pretermino"))

birth %<>% filter(is.na(AgeMother)==F)


birth %>% ggplot(aes(x=AgeMother, fill= Term))+
  geom_bar(position = "dodge")+labs(x="Rangos de edad de las madres\ndel dia del nacimiento del RN",
                                    y="Frecuencia", title = "# de Embarazos según los rango de edad de las madres")+
  scale_fill_discrete(name = "Clasificador del RN")

