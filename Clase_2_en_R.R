# ==========================
# Operaciones básicas en R:
# ==========================

# Suma:
4+3
7+4
3+0.2

# Resta:
4-2
5-0.2
3-9

# Multiplicación:
5*9
4*6
2*0.3

# División:
9/3
5/2
7/3

# Residuo:
6%%2
6%%5
11%%3

# Cociente:
5%/%2
7%/%3
17%/%5

# Exponente:
5^2   # Forma 1
5**2  # Forma 2
6^3

# Exponente negativo:
4^(-2)
5^(-3)

# =============================
# Información sobre la sesión:
# =============================
sessionInfo()

# ============================
# Objetos en mi sesión:
# ============================

ls()  # Objetos activos - Forma 1
objects()  # Objetos activos - Forma 2

rm("x1") # Eliminando un objeto

rm(list=ls()) # Eliminando todos los objetos

# ========================================
# En cuál carpeta está  activa mi sesión:
# ========================================

getwd()

# ========================================
# Fijando una carpeta para mi sesión:
# ========================================

setwd("C/Users/Escritorio")

# ====================
# Creando variables:
#=====================

x_1 = 3  # Forma 1
x_1 <-   # Forma 2 
3 -> x_1 # Forma 3

# ====================
# Creando vectores:
#=====================

v_1 <- c(1,9,-2)
v_2 <- c(8, 3, 9)

# Suma de vectores:
v_1 + v_2


# Resta de vectores:
v_1 - v_2

# ====================
# Creando matrices:
#=====================

# Forma 1:
m_1 <- matrix(c(7,3,2,5,-2,17), nrow=3, ncol=2)
m_1

# Forma 2:
c1 <- c(7, 3, 2)
c2 <- c(5, -2, 17)
m_1 <- cbind(c1, c2)
m_1

# Forma 3:
r1 <- c(7,5)
r2 <- c(3,-2)
r3 <- c(2,17)
m_1 <- rbind(r1, r2, r3)
m_1

# =======================
# Creando data frames:
# =======================

col1 <- c(3, 5, 6, 7, 11)
col2 <- c("a", "f", "h", "a", "d")
col3 <- c(1.7, 2.1, 4.2, 6.7, 3.5)
df_1 <- data.frame( col1, col2, col3) 
df_1

# No es un data frame:
m_2 <- cbind(col1, col2, col3)
m_2

str(df_1) #  Estructura de un data frame

# ===============================
# Operadores de comparación:
# ===============================
a<-1 
b<-3
b!=a # ¿b es diferente de a?
isTRUE(a==b) # ¿es a igual a b? 
!(a<b) # negar que a es menor que b 
(a<b | b<a) # ¿es a menor que b o b menor que a?
(a<=b & b==a) #¿es a menor o igual a b o b igual a a?

# ===============================
# Condicional if-else:
# ===============================

a<-9
if (a<0){ 
  print("a es negativo")
  }else if (a>0){
    print("a es positivo")
}else{ print("a es igual a cero")
}

# ===============================
# Bucles for:
# ===============================

# Ejemplo 1:
suma<-0 
for (i in 1:10){ 
  suma<-suma+i 
  }
suma

# Ejemplo 2:
lista<-c("a","b","c","d") 
for (j in lista){
  print(j) }

# Ejemplo 3:
vector<-c(0.1, 0.3, -0.4, 1.5, -2.1)
for (k in vector){
  print(abs(k)) }

# Ejemplo 4:
x1<-cbind(1:3,4:6) # Matriz 1
x2<-cbind(3:5,4:6) # Matriz 2
x3<-cbind(5:7,9:11) # Matriz 3
l1<-list(x1,x2,x3) # Lista de matrices
for(i in l1){
  print(i) }

# Ejemplo 5:
for (i in 1:10){ 
  if (! i % %2){ next
  }
  print(i) }

# ===============================
# Bucles while:
# ===============================

# Ejemplo 1:
a<-1 
while (a<5){ 
  a<-a+1
  print(a)
  }

# Ejemplo 2:
a<-1
b<-3 
while (a<15 & b>0.2){
  a<-a+1 
  b<-round(b/2,2)
  print(c(a,b)) 
  }

# ===============================
# Bucles repeat:
# ===============================

a<-1
b<-3
repeat{
  a<-a+1 
  b<-round(b/2,2)
  print(c(a,b)) 
  if(a>15 | b<0.2){ break
  }
}

# ===============================
# Funciones:
# ===============================

# Ejemplo 1:
sumar <- function(m){
  sum1<-0 
  for (i in 1:m){
    if(! i %% 2){ next 
    }else{ 
        sum1 <-sum1+i}
  }
  return(sum1) 
}

sumar(8)

# Ejemplo 2:
multiplos<-function(a,b,n){ 
  a1<-vector() #Múltiplos de a menores o iguales a n
  b1<-vector() #Múltiplos de b menores o iguales a n 
  ca1<-1
  cb1<-1
  for (i in 1:n){ 
    if(!i %%a){ 
      a1[ca1]<-i
      ca1<-ca1+1
}
if(!i %%b){
  b1[cb1]<-i 
  cb1<-cb1+1
}else{ next
} }
  return(list(Multiplos_a=a1,Multiplos_b=b1))
  }

multiplos(3,5,20)

