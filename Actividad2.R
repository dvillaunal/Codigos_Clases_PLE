b <- sample(15:20,10, replace = T)
q <- c("Pedro", "Juan", "Gabriel", "Felipe","Daniel",
       "Hector", "Jorge", "Raul", "Tito", "Saul")
df <- data.frame("Nombre"=q,"Edad"=b)

filtro <- function(d,x,v){
  # esta función filtra de forma arcaíca (indexación)
  if(is.numeric(x)==TRUE){
    print("Identificado: La variable x es un numero")
    nd <- d[d[,x]==v,]
    return(nd)
  }
  nd <- d[d[,x]==v,]
  return(nd)
}

filtro(df,2,19)

filtroM <- function(d,x,v){}


