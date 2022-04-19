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
