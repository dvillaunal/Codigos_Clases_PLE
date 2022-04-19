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



