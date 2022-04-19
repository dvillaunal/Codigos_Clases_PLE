require(magrittr)
require(tidyverse)
require(janitor)
require(datetime)

direccion <- "Delito_Hurto_Motocicletas_limpia.csv"

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
  geom_bar(aes(x=dia,fill=dia), alpha=0.4)+
  theme(legend.position = "none")

datos1 %>% ggplot()+
  geom_density(aes(x=hora, fill="red"), alpha=0.4)+
  scale_x_discrete(limits=0:24)+
  theme(legend.position = "none")+
  labs(title = "Reporte de hurto de motocicletas a lo largo del día")
  

