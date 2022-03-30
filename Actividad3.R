vendedor <- c("1", "2", "3", "4", "5", "6")
conocimiento <- c(8, 9, 7, 8, 6, 8)
experiencia <- c(5, 2, 2, 1, 4, 4)
concepto <- c("bueno", "bueno", "malo", "malo", "bueno", "bueno")
ventas <- c(54, 50, 48, 32, 30, 30)
zona <- c("norte", "sur", "sur", "oeste", "sur", "oeste")

datos <- data.frame(vendedor, conocimiento, experiencia,
                    concepto, ventas, zona)

#str(datos)
datos$concepto <- as.factor(datos$concepto)
levels(datos$concepto)
datos$zona <- as.factor(datos$zona)
#summary(datos)


datos_1 <- datos[order(datos$conocimiento, decreasing = TRUE),]

#?order

vendedor <- 7:15
conocimiento <- c(5,5,6,7,4,7,3,5,3)
experiencia <- c(3, 3, 1, 3, 4, 2, 3, 1, 2)
concepto <- c("bueno", "bueno", "malo", "malo",
              "bueno", "malo", "malo", "malo",
              "bueno")
ventas <- c(29, 27, 24, 24,24, 23, 21, 21, 16)
zona <- c("norte", "norte","oeste", "oeste",
          "sur", "norte", "sur", "oeste", "norte")

datos2 <- data.frame(vendedor, conocimiento,
                     experiencia, concepto, ventas, zona)

datos_u <- rbind(datos, datos2)
#str(datos_u)

mu <- mean(datos_u$ventas)
v <- var(datos_u$ventas)
ds <- sd(datos_u$ventas)

datos_u[datos_u$ventas > mu+2*ds,]

datos_u$ventasp <- round(prop.table(datos_u$ventas)*100,2)

# Por:
# conocimiento
# experiencia
# concepto
library(ggplot2)
ggplot(datos_u, aes(x=concepto, y=ventas, fill=zona)) +
  geom_col(position = "dodge")+labs(y = "Ventas")


prop.table(table(datos_u$zona))

xtabs(ventas ~ conocimiento, data = datos_u)

ggplot(datos_u, aes(x=conocimiento, fill = zona))

quantile(datos_u$conocimiento, probs = 0.25)

datos_u[datos_u$conocimiento <= 5,]
