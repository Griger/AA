#FUNCIONES DE BASE RADIAL

#Funcion que devuelve un n??cleo Gaussiano normalizado para R^d
fiD <- function(d){
  function(z) {exp(-0.5 * z^2)/((2*pi)^(-d/2))}
}

RBF <- function(x, datos.train, et.train, r) {
  N = nrow(datos.train) #num muestras train
  fi <- fiD(ncol(datos.train))
  alfas = apply(datos.train, 1, function(y) { fi(dist(y,x)/r) } )
  sum(alfas*et.train)/sum(alfas) #g(x)
}