mode <- function(x) {
  # Estimación unimodal discreta
  # La moda se calcula como el valor más repetido dentro de la variable
  return(as.numeric(names(which.max(table(x)))))
}