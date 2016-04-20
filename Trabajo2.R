library('MASS')
set.seed(41192)
#Ejercicio 1.1
#Definimos una funcion que calcule la distancia euclidea entre dos vectores
d <- function(x,y) {
  sqrt(sum((x-y)**2))
}

#Definimos la fun de error
E <- function(x) {
  u = x[1]
  v = x[2]
  (u*exp(v)-2*v*exp(-u))**2
}

#Definimos el gradiente de la fun de error anterior
gradE <- function(x) {
  u = x[1]
  v = x[2]
  2*(u*exp(v)-2*v*exp(-u))*c(exp(v)+2*v*exp(-u), u*exp(v)-2*exp(-u))
}

'
Metodo general gradiente descendente
@f: funcion a minimizar
@gradf: gradiente de la funcion f
@eta: tasa de aprendizaje
@w0: punto de inicio
@tol: valor hasta donde queremos llegar con la minimizacion
@max_iter: maximo de iteraciones permitidas para el algoritmo
'
gradDesc <- function(f, gradf, eta, w0, tol, max_iter = 1000000, dibujar = FALSE) {
  w = w0 #inicializamos pesos
  n_iters = 1
  valores_f = NULL
  
  repeat {
    gt = gradf(w) #calculamos el gradiente
    vt = -gt
    w_ant = w 
    f_ant = f(w)
    valores_f = c(valores_f, f_ant)
    w = w + eta*vt #actualizamos el vector de pesos
    n_iters = n_iters+1
    
    #cond. de parada
    if (f(w) < tol || n_iters == max_iter || d(w_ant, w) <= tol || abs(f(w) - f_ant) <= tol){
      valores_f = c(valores_f, f(w))
      break
    } 
  }
  
  if (dibujar)
    plot(seq(n_iters), valores_f, type = "l", ylab = "Valor de f", xlab = "Iteraciones")
  
  cat("Num de iters empleado", n_iters, "\n")
  cat("Valor de f alcanzado", f(w), "\n")
  cat("w obtenido", w, "\n")
  w
}

#Definimos la fun f del 1.1.b
f <- function(X){
  x = X[1]
  y = X[2]
  
  x**2+2*y**2+2*sin(2*pi*x)*sin(2*pi*y)
}

#Definimos su gradiente
gradf <- function(X) {
  x = X[1]
  y = X[2]
  
  c(2*x + 4*pi*sin(2*pi*y)*cos(2*pi*x), 4*y + 4*pi*sin(2*pi*x)*cos(2*pi*y))
}

#gradDesc(E, gradE, 0.1, c(1,1), 10**(-14)) (llamada para el ejercicio 1.1)

#Ejercicio 1.2

'
Metodo general coordenada descendente
@f: funcion a minimizar
@gradf: el gradiente de la funcoin a minimizar
@eta: tasa de aprendizaje
@w0: punto de inicio
@max_iter: maximo de iteraciones permitidas para el algoritmo
'
coordDesc <- function(f, gradf, eta, w0, tol, max_iter = 1000000) {
  w = w0
  n_iters = 1
  
  repeat{
    w_ant = w 
    f_ant = f(w)
    n_iters = n_iters + 1
    
    w[1] = w[1] - eta*gradf(w)[1]
    w[2] = w[2] - eta*gradf(w)[2]
    
    if (f(w) < tol || n_iters == max_iter || d(w_ant, w) <= tol || abs(f(w) - f_ant) <= tol)
      break
    
  }
  
  list(w, f(w))
}

#Ejercicio 1.3

#definimos la hessiana de la fun. f
Hf <- function(X) {
  x = X[1]
  y = X[2]
  
  matrix(c(2-8*pi*pi*sin(2*pi*y)*sin(2*pi*x), 8*pi*pi*cos(2*pi*y)*cos(2*pi*x), 
           8*pi*pi*cos(2*pi*x)*cos(2*pi*y), 4-8*pi*pi*sin(2*pi*x)*sin(2*pi*y)),
         2,2)
}

'
Metodo de Newton
@f: funcion a minimizar
@gradf: el gradiente de la fun. a minimizar
@Hf: matriz hessiana de la fun. a minimizar
@eta: tasa de aprendizaje
@w0: punto de inicio
@max_iter: maximo de iteraciones permitidas para el algoritmo
'
Newton <- function(f, gradf, Hf, eta, w0, max_iter){
  w = w0
  n_iters = 0
  while (n_iters < max_iter) {
    w = w-t(eta*ginv(Hf(w))%*%gradf(w))
    n_iters = n_iters + 1
  }
  
  w
}

#Ejercicio 1.4

