#-----Actividad N°5-------
library(tidyverse)
library(magrittr)
require(datetime)
require(janitor)


df <-  read_csv("Delito_Hurto_Motocicletas_limpia.csv")

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

