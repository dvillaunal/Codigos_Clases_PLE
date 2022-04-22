#-------Actividad N°9--------
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
  
  
  