# Daniel Felipe Villa Rengifo
# CC. 1005087556

#-----Actividad N°2------

# Base de datps de usuarios anonimos, ¿Cuantas veces te cepillas al día?
b <- sample(1:4,2600, replace = T)
q <- rep(letters,100)
df <- data.frame("anonimo"=q,"cepillado"=b)

filtro <- function(d,x,v){
  # esta función filtra de forma arcaíca (indexación)
  if(is.numeric(x)==TRUE){
    nd <- d[d[,x]==v,]
    return(nd)
  }
  nd <- d[d[,x]==v,]
  return(nd)
}

# Función para 1 variable
filtro(df,2,3)


# Para varias Variables:
multi.filtro <- function(d,x,v){
  lista <- list()
  c = 1
  for (i in x) {
    lista[[c]] <- filtro(d,i,v[c])
    c = c+1
  }
  return(lista)
}

v <- sample(1:4,4,replace = F)
x <- rep(2,4)
lista <- multi.filtro(df,x,v)

#----Actividad N°3-----

library(tidyverse)
library(magrittr)
# Punto 1
vendedor <- c("1", "2", "3", "4", "5", "6")
conocimiento <- c(8, 9, 7, 8, 6, 8)
experiencia <- c(5, 2, 2, 1, 4, 4)
concepto <- c("bueno", "bueno", "malo", "malo", "bueno", "bueno")
ventas <- c(54, 50, 48, 32, 30, 30)
zona <- c("norte", "sur", "sur", "oeste", "sur", "oeste")

datos <- data.frame(conocimiento, experiencia,
                    concepto, ventas, zona,
                    stringsAsFactors = T, row.names = vendedor)

datos
# Punto 2
vendedor <- 7:15
conocimiento <- c(5,5,6,7,4,7,3,5,3)
experiencia <- c(3, 3, 1, 3, 4, 2, 3, 1, 2)
concepto <- c("bueno", "bueno", "malo", "malo",
              "bueno", "malo", "malo", "malo",
              "bueno")
ventas <- c(29, 27, 24, 24,24, 23, 21, 21, 16)
zona <- c("norte", "norte","oeste", "oeste",
          "sur", "norte", "sur", "oeste", "norte")

datos2 <- data.frame(conocimiento, experiencia,
                     concepto, ventas, zona,
                     stringsAsFactors = T, row.names = vendedor)

datos_u <- rbind(datos, datos2)
datos_u

# Punto 3
## a:
mu <- mean(datos_u$ventas)
mu
v <- var(datos_u$ventas)
v
ds <- sd(datos_u$ventas)
ds

datos_u[datos_u$ventas > mu+2*ds,]

## b:

datos_u %>% xtabs(ventas~experiencia+zona,.) %>%
  as_tibble() %>% ggplot(aes(x=experiencia,y=n, fill=zona))+geom_col()+
  facet_grid(~zona)+labs(y="# de Ventas", x="Experiencia en años")+
  guides(fill = FALSE) # to remove the legend

# c:

datos_u %>% 
  filter(conocimiento < quantile(datos_u$conocimiento,0.25))

#----Actividad N°4-----

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

#----Actividad N°5-----

library(tidyverse)
library(magrittr)
require(datetime)
require(janitor)


df <-  read_csv("datos/Delito_Hurto_Motocicletas_limpia.csv")

df$ESTADO.CIVIL %>% table() %>% sort(decreasing = T) %>%
  as.data.frame()

# Respueta 1: Soltero

df$MOVIL.AGRESOR %>% table() %>% sort(decreasing = T) %>%
  as.data.frame()

# Respueta 2: A pie
data.c <- df$COLOR %>% table() %>%  as.data.frame()

names(data.c)[1] <- "color"

str_match(data.c$color,"NEGRO") %>% table()
str_match(data.c$color,"AZUL") %>% table()
# Respueta 3: Negro = 5 | Azul = 8

df$GENERO %>% table()

# Respueta 4: Femenino

df$MUNICIPIO %<>% as.factor()

df$MUNICIPIO %>% table() %>% sort(decreasing = T)

# Respuesta 5: Medellín

df$DIA %<>% as.factor()

df$DIA %>% table() %>% sort(decreasing = T)

# Respueta 6: Miercoles

df$FECHA %<>% strptime(format=" %d/ %m/ %Y %H: %M") %>% as.Date()

df$MES <- df$FECHA %>% months()

df$MES %>% table() %>% sort(decreasing = T)

# Respuesta 7: Noviembre

age <- df$EDAD %>% table() %>% as.data.frame()

names(age)[1] <- "edad"

age %>% filter(Freq >= 800) %>% ggplot(aes(x=edad, y=Freq, fill=edad))+
  geom_col()+theme(legend.position = "none")+
  geom_text(aes(label=Freq), position="dodge", vjust="inward")+
  labs(y="Frecuencia", x= "Edad",
       title = "Conteo de las edades de los denunciantes",
       subtitle = "Se tomaron las primeras 16 observaciones < 800")

df$HORA %>% table() %>% sort(decreasing = T)

hora <- df$HORA %>% as.hour.minute()/60 %>% as.numeric()

hora <- cut(hora,breaks=0:24, include.lowest = T)

hora %>% table() %>% sort(decreasing = T)

# Respuesta 9: En el transcurso de las 7 PM y las 8 PM

orden_dia <- c("Lunes", "Martes", "Miércoles", "Jueves",
               "Viernes", "Sábado", "Domingo")
df$DIA %<>% factor(levels = orden_dia)

df %>% ggplot()+
  geom_density(aes(x=HORA, fill=DIA), alpha=0.4)+facet_grid(~DIA)+
  theme(legend.position = "none")+
  labs(title = "Reporte de hurto de motocicletas por hora\nfacetado por días")

#---Actividad N°7----

library(readr)
#Importar la bases de datos
db1 <- read_csv("datos/DB1.csv")
db2 <- read_csv("datos/DB2.csv")
db3 <- read_csv("datos/DB3.csv")
db4 <- read_csv("datos/DB4.csv")

# dimensiones
print("Dimensiones db1")
dim(db1)
print("Dimensiones db2")
dim(db2)
print("Dimensiones db3")
dim(db3)
print("Dimensiones db4")
dim(db4)

#Estructura
print("Estructura db1")
str(db1)
print("Estructura db2")
str(db2)
print("Estructura db3")
str(db3)
print("Estructura db4")
str(db4)

# Unir las bases de datos:
db.1 <- merge(db1,db2)
db.2 <- merge(db3,db4)

vehiculos <- merge(db.1,db.2)

vehiculos

#----Actividad N°10-----
require(readxl)
require(magrittr)
require(tidyverse)
require(janitor)
require(datetime)

direccion <- "datos/Delito_Hurto_Motocicletas_limpia.csv"

datos1<-read_csv(direccion)

datos1 %<>% clean_names()

datos1$fecha %<>% strptime(format=" %d/ %m/ %Y %H: %M") %>% as.Date()

datos1$mes <- datos1$fecha %>% months()

datos1$hora %<>% as.hour.minute()

datos1$hora <- datos1$hora/60

datos1$modelo[ datos1$modelo<1963 | datos1$modelo>2017 ] <- NA

datos1 %<>% mutate_if(is.character, as.factor )

orden_dia <- c("Lunes", "Martes", "Miércoles", "Jueves",
               "Viernes", "Sábado", "Domingo")

datos1$dia %<>% factor(levels = orden_dia)

orden_mes <- c("enero","febrero","marzo","abril","mayo","junio",
               "julio","agosto","septiembre","octubre",
               "noviembre","diciembre")

datos1$mes %<>% factor(levels = orden_mes)

datos1 %>% ggplot()+
  geom_density(aes(x=hora, fill=dia), alpha=0.4)+facet_grid(~dia)+
  theme(legend.position = "none")+
  labs(title = "Comportamiento de los hurtos en los días de la semana")

datos1 %>% ggplot()+
  geom_density(aes(x=hora, fill="red"), alpha=0.4)+
  scale_x_discrete(limits=0:24)+
  theme(legend.position = "none")+
  labs(title = "Reporte de hurto de motocicletas a lo largo del día")

# omito un dato ya que no reporto genero
datos1 %>% filter(.,genero!="NO REPORTADO") %>%  ggplot(aes(x=genero, fill= genero))+
  geom_bar()+theme(legend.position = "none")+
  labs(title = "# de Robos, discriminado por genero",y="Frecuencia")

datos1 %>%  ggplot(aes(x=zona, fill= zona))+
  geom_bar()+theme(legend.position = "none")+
  labs(title = "# de Robos según la zona",y="Frecuencia")

#Crear rangos de dos horas para una mejor visualización de los datos.

ant <- datos1 %>% filter(.,departamento=="ANTIOQUIA")
ant$municipio %<>% as.character()

ant$cat.hora <- ant$hora %>% cut(breaks=c(0,12,5,24), labels=c("Mañana", "Tarde", "Noche"))

ant10 <- ant %>% count(municipio,sort = T)  %>% filter(n>=33)

diez <- ant10$municipio

ant2 <- ant %>% filter(municipio %in% diez[1:5]) %>%
  xtabs(~municipio+cat.hora,.) %>% as.data.frame()

ant2 %>% ggplot(aes(x=municipio, y= Freq, fill=cat.hora))+
  geom_col(position = "dodge")+
  labs(title = "# de Robos de municipios de Antioquia",
       subtitle = "(los 5 municipios que más presentan denuncias)",
       y="Frecuencia")+
  guides(fill = guide_legend(title = "Categorización\nde las horas del día"))+
  scale_fill_hue(labels = c("Mañana 0AM-12PM", "Tarde 12PM-5PM", "Noche 5PM-23:59PM"))+
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "white"))

# Lectura de la base de datos:
licores <- read_excel("datos/licores.xlsx")

# TL: Tipo de licor
# PI: Precio de incautación. Se refiere al precio de venta en
# el establecimiento por unidad.
# GAE: Grados de alcohol en etiqueta.
# GAQ: Grados de alcohol en prueba química.
# CE: Cantidad estandarizada. Número de unidades estandarizadas a 750 ml.

licores$TL %<>% str_replace("Aguardientre","Aguardiente")

licores$TL %>% unique()

reemplazos <- c("Run"="Ron","Whiski"="Whisky")
licores$TL %<>% str_replace_all(., reemplazos ) %>% as.factor()
licores$TL %>% unique()

licores$PI[licores$PI>100000] <- NA
licores$CE[licores$CE<200] <- NA

medias <- licores %>% dplyr::select(-TL) %>% colMeans(na.rm=TRUE)
medias

# Creamos una lista con los reemplazos:
reemplazos<- list(PI=medias[1], GAE=medias[2],
                  GAQ=medias[3], CE=medias[4])

# Reemplazamos y guardamos en datos1:
datos1 %<>% replace_na(reemplazos)

reemplazos<- list(TL="Aguardiente")

datos1 %<>% replace_na(reemplazos)

# ¿Que licor es el que tiene más publicidad engañosa
# respecto a los grados de licor?

licores %<>% drop_na()

licores %>% ggplot()+
  geom_density(aes(x=GAE, fill= "etiqueta"), alpha=0.5)+
  geom_density(aes(x=GAQ, fill= "prueba quimica"), alpha=0.5)+
  facet_grid(~TL)+
  guides(fill = guide_legend(title = ""))+
  labs(x="Grados de Alcohol en %",
       title = "Comparación de los grados de alcohol\nfacetado por el tipo de licor")

# Observar como se distribuyen los precios según el tipo de licor

licores %>% ggplot(aes(x=TL,y=PI, fill=TL))+geom_violin(alpha=0.7)+
  theme(legend.position = "none")

# Ver un Histograma de los precios:

licores %>% ggplot(aes(x=PI))+
  geom_histogram(bins=10,alpha=0.7, color="black")+
  labs(title = "Histograma del precio de incautación",x="Precio de Incautación",
       y="Frecuencia")


# Cual el es el dato más predomimante en % por tipo de licor
licor <- round(prop.table(table(licores$TL))*100,2) %>% as.data.frame()

licor %>% ggplot(aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = paste(Freq,"%")),
            position = position_stack(vjust = 0.4))+
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Piechart de los % según el tipo de licor")+
  scale_fill_discrete(name = "Tipo de Licor")

#-----BD Agrícola----


library(tidyverse)
library(magrittr)
library(janitor)
library(datetime)
library(knitr)
evagro <- read_csv("datos/Evaluaciones_Agropecuarias_Municipales_EVA.csv")

evagro %<>% clean_names()

evagro$departamento %<>% as.factor()

evagro$municipio %<>% as.factor()

evagro$grupo_de_cultivo %<>% as.factor()

evagro$subgrupo_de_cultivo %<>% as.factor()

evagro$cultivo %<>% as.factor()

evagro$desagregacion_regional_y_o_sistema_productivo %<>% as.factor()

evagro$periodo %<>% as.factor()

evagro$estado_fisico_produccion %<>% as.factor()

evagro$nombre_cientifico %<>% as.factor()

evagro$ciclo_de_cultivo %<>% as.factor()

evagro$ano %<>% as.factor()


evagro <- evagro[!is.na(evagro$rendimiento_t_ha),]

# a
evagro %>% count(ano, sort=T) %>% head(n=1) %>% knitr::kable() 

#b 
evagro %>% filter(ano==2010) %>%
  count(departamento, sort = T) %>% head(n=1) %>% knitr::kable() 

#c
evagro %>% filter(ano==2015) %>%
  count(municipio, sort = T) %>% head(n=1) %>% knitr::kable() 

# d
evagro %>% filter(ano==2017) %>%
  count(cultivo, sort = T) %>% head(n=1) %>% knitr::kable() 

#e
evagro %>% filter(departamento=="BOYACA" & cultivo == "ARVEJA") %>% 
  count(cultivo) %>% knitr::kable()


#f
papa <- evagro %>% filter(departamento=="BOYACA" & cultivo=="PAPA") %>%
  aggregate(produccion_t~ano,.,FUN = sum)

papa %<>% filter(ano %in% c("2015", "2016"))

paste(diff(papa$produccion_t), "Toneladas")

#g
evagro %>% filter(departamento=="ANTIOQUIA" & cultivo=="PAPA") %>%
  count(ano, sort = T) %>% head(1) %>% kable()


# h

evagro %>% count(estado_fisico_produccion, sort = T) %>% 
  head(1) %>% kable()

#i
evagro %>% filter(cultivo=="BANANO") %>% count(ano,sort = T) %>% 
  head(1) %>% kable()

#j

sub15 <- evagro %>% filter(ano=="2015") %>%
  aggregate(area_sembrada_ha~subgrupo_de_cultivo,.,FUN=sum)

sub15[order(sub15$area_sembrada_ha, decreasing = T),] %>% head(1) %>% 
  kable()


# k
library(GGally)

evagro %>% filter(departamento=="CUNDINAMARCA")  %>%
  dplyr::select(area_sembrada_ha, area_cosechada_ha) %>% ggpairs()

# l

sub16 <- evagro %>% filter(ano=="2016" & departamento=="CHOCO") %>%
  aggregate(area_sembrada_ha~subgrupo_de_cultivo,.,FUN=sum)

sub16[order(sub16$area_sembrada_ha, decreasing = T),] %>% head(1) %>% 
  kable()

#m

sub13 <- evagro %>% filter(ano=="2013" & departamento=="META") %>%
  aggregate(area_sembrada_ha~subgrupo_de_cultivo,.,FUN=sum)

sub13[order(sub13$area_sembrada_ha, decreasing = T),] %>% head(1) %>% 
  kable()


# n

san15 <- evagro %>% filter(departamento=="SAN ANDRES Y PROVIDENCIA" & ano=="2015") %>% 
  aggregate(area_sembrada_ha~subgrupo_de_cultivo,.,FUN=sum)

san15[order(san15$area_sembrada_ha, decreasing = T),] %>% head(1) %>% 
  kable()

# o
# Mayor rendimiento promedio
year <- levels(evagro$ano)[2:13]

rendi <- evagro %>%  filter(ano %in% year) %>% 
  aggregate(rendimiento_t_ha~subgrupo_de_cultivo,.,FUN=mean)

rendi <- rendi[order(rendi$rendimiento_t_ha, decreasing = T),]

rendi$rendimiento_t_ha %<>% round(.,0)

rendi %>%  head(1) %>% kable()

#p
# Rendimiento promedio

rendi %>% filter(rendimiento_t_ha==1) %>% kable()


# q

valle <- evagro %>%  filter(ano %in% year & departamento=="VALLE DEL CAUCA") %>% 
  aggregate(rendimiento_t_ha~subgrupo_de_cultivo,.,FUN=sum)


valle <- valle[order(valle$rendimiento_t_ha, decreasing = T),]

valle %>% head(1) %>% kable()

# r
year <- levels(evagro$ano)[5:13]

perm <- evagro %>%
  aggregate(area_sembrada_ha~departamento+ciclo_de_cultivo,.,FUN=sum) %>% 
  filter(ciclo_de_cultivo=="PERMANENTE")

perm <- perm[order(perm$area_sembrada_ha, decreasing = T),]

perm %>% head(1) %>% kable()

# s

year <- levels(evagro$ano)[10:13]

ciclo <- evagro %>% filter(departamento=="ATLANTICO" & ano %in% year) %>% 
  aggregate(area_sembrada_ha~ciclo_de_cultivo,.,FUN = sum)

ciclo <- ciclo[order(ciclo$area_sembrada_ha, decreasing = T),]

ciclo %>% head(1) %>%  kable()

# t

muni <- evagro %>% filter(ano=="2014") %>%
  aggregate(area_sembrada_ha~municipio+ciclo_de_cultivo,.,FUN = sum)

muni <- muni[order(muni$area_sembrada_ha, decreasing = T),] %>%
  filter(ciclo_de_cultivo=="PERMANENTE")

muni %>% head(1) %>% kable()

