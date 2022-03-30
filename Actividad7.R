library(readr)
#Importar la bases de datos
db1 <- read_csv("datasets_class7/DB1.csv")
db2 <- read_csv("datasets_class7/DB2.csv")
db3 <- read_csv("datasets_class7/DB3.csv")
db4 <- read_csv("datasets_class7/DB4.csv")

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
db <- c(db1,db2,db3,db4)


merge(db1,db2)
merge(db3,db4)
