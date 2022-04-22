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
  select(.,area_sembrada_ha, area_cosechada_ha) %>% ggpairs()

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
