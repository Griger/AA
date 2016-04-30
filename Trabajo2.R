library('MASS')
set.seed(41192)

#Funciones externas
simula_unif <- function (N, dim, rango) {
  datos <- matrix(runif(N*dim, rango[1], rango[2]), ncol = dim, nrow = N)
  datos
}

simula_recta <- function(rango = c(-50,50)) {
  coordenadas = simula_unif(2,2,rango) #generamos dos ptos en el cuadrado rangoxrango
  a = (coordenadas[2,2]-coordenadas[1,2])/(coordenadas[2,1]-coordenadas[1,1])#la pendiente (m)
  b = -a*coordenadas[1,1] + coordenadas[1,2] #el factor independiente en la ecuacion y = mx+b
  
  c(a,b)
}

f0 <- function(a, b, x, y) {
  s = sign(y-b*x-a)
  if (s == -1)
    return(0)
  else 
    return(1)
}

getImagen <- function(datos) {
  matrix(datos, ncol = 16) #le pasamos los datos y las columnas que tiene que tener la matriz que formemos con ellos
}

getSimetria <- function(m) {
  -2*sum(abs(m[,seq(1,8)]-m[,seq(16,9)]))
}

h <- function(w, dato) {
  sign(w%*%dato) #signo del producto escalar entre w y el dato
}

plotw <- function(w, color = 'black') {
  if (w[2] != 0) #si podemos despejar el termino que va con y
    abline(a = -w[1]/w[2], b = -w[3]/w[2], col = color)
  else if (w[1] != 0) #si lo que tenemos es una recta vertical
    abline(v = -w[3]/w[1], col = color)
  else
    print("No hay linea.")
}

ajusta_PLA_MOD <- function(datos, label, max_iter, vini) {
  w <- vini
  mejor_w <- w
  num_datos <- nrow(datos)
  min_errores <- num_datos
  encontrado = FALSE
  iter = 1
  
  while (!encontrado && iter < max_iter) {
    encontrado <- TRUE
    
    for (i in seq_len(num_datos))
      if (h(w, datos[i,]) != label[i]) {
        w <- w + label[i]*datos[i,]
        
        #por cada modificacion contamos el numero de errores del nuevo w
        errores_w = length(which(label != apply(datos, 1, h, w = w)))
        
        #si es menor que el que teniamos el mejor w encontrado se actualiza con el nuevo
        if (errores_w < min_errores) {
          mejor_w <- w
          min_errores = errores_w
        }
        
        encontrado <- FALSE
      }
    
    iter = iter + 1
  }
  
  
  list(mejor_w,iter)
}

#funcion que calcula la recta de regresion con las formulas vistas en clase
regressLin <- function(datos, label) {
  #obtenemos la descomposicion SVD de la matriz
  svd(datos) -> S
  
  #fabricamos una matriz diagonal con la diagonal del SVD redondeando los valores
  diag(round(S$d, digits = 5)) -> D
  #calculamos la pseudo-inversa de la matriz D con el ginv del paquete MASS
  ginv(D) -> gD
  #calculamos (t(datos) * datos)^-1 con la formula vista en clase V*D^-2*t(V)
  S$v%*%gD%*%gD%*%t(S$v) -> A
  #obtenemos el vector que nos da el hiperplano de regresion con la formula w = (t(X)X)^-1t(X)y
  A%*%t(datos)%*%label -> wlin
  
  wlin
}

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
  
  cat("Num de iters empleado", n_iters, "\n")
  cat("Valor de f alcanzado", f(w), "\n")
  cat("w obtenido", w, "\n")
  
  if (dibujar)
    plot(seq(n_iters), valores_f, type = "l", ylab = "Valor de f", xlab = "Iteraciones")
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
'
gradDesc(f, gradf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
gradDesc(f, gradf, 0.1, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
'

'
gradDesc(f, gradf, 0.01, c(0.1,0.1), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(-0.5,-0.5), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(-1,-1), 10**(-14), max_iter = 50, dibujar = FALSE)
'
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
    
    if (f(w) < tol || n_iters == max_iter || d(w_ant, w) <= tol || abs(f(w) - f_ant) <= tol) break
    
  }
  
  cat("Iteraciones empleadas: ", n_iters, "\n")
  list(w, f(w))
}

#coordDesc(E, gradE, 0.1, c(1,1), 10**(-14), 15) (llamada ej. 1.2)

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
Newton <- function(f, gradf, Hf, eta, w0, tol, max_iter = 1000000, dibujar = FALSE){
  w = w0
  n_iters = 1
  valores_f = NULL
  
  repeat {
    w_ant = w 
    f_ant = f(w)
    
    valores_f = c(valores_f, f_ant)
    w = w-t(eta*ginv(Hf(w))%*%gradf(w))
    n_iters = n_iters + 1
    
    if (f(w) < tol || n_iters == max_iter || d(w_ant, w) <= tol || abs(f(w) - f_ant) <= tol){
      valores_f = c(valores_f, f(w))
      break
    } 
  }
  
  if (dibujar)
    plot(seq(n_iters), valores_f, type = "l", ylab = "Valor de f", xlab = "Iteraciones")
  
}

#Newton(f, gradf, Hf, 0.1, c(1,1), 10**(-14), 50) (llamada ejemplo para ej. 1.3)

#Ejercicio 1.4
'
Regresion Logistica
@datos: los datos a los que ajustarnos
@et: las etiquetas de los datos
@w0: vector de pesos inicial
@eta: tasa de aprendizaje
@tol: la diferencia entre dos vectores de pesos consecutivos para la cond. de parada
'
RL <- function(datos, et, eta, w0, tol) {
  N = nrow(datos)
  w = w0
  
  repeat{
    w_ant = w
    
    for (i in sample(N)) {
      gt = (-et[i]*datos[i,])/(1+exp(et[i]*w%*%datos[i,]))
      w = w - eta*gt
    }
    
    if (d(w, w_ant) < tol)
      break
  }
  
  w
}

ejercicio1.4 <- function(){
  err_out_medio = 0
  n_experimentos = 100
  
  for (i in seq(n_experimentos)) {
    datos <- simula_unif(100, 2, c(-1,1))
    datos_out <- simula_unif(100,2,c(-1,1))
    
    recta <- simula_recta(c(-1,1))
    ets <- mapply(f0, recta[2], recta[1], datos[,1], datos[,2])
    ets_out <- mapply(f0, recta[2], recta[1], datos_out[,1], datos_out[,2])
    
    w = RL(datos, ets, 0.01, c(0,0), 0.01)
    ets_reg_out <- apply(datos_out, 1, h, w = w) #obtenemos las etiquetas que el vector dado por la regresion le da a los datos fuera de la muestra
    ets_reg_out[which(ets_reg_out == -1)] = 0

    err_out_medio = err_out_medio + length(which(ets_reg_out != ets_out))
  }
  
  err_out_medio/n_experimentos
}

#Ejercicio 1.5
#datos_train <- read.table("datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
#datos_test <- read.table("datos/zip.test", quote="\"", comment.char="", stringsAsFactors=FALSE)
datos_test <- read.table("/media/griger/Datos/Documentos/Facultad/5DGIIYM/AA/P/AA/datos/zip.test", quote="\"", comment.char="", stringsAsFactors=FALSE)
datos_train <- read.table("/media/griger/Datos/Documentos/Facultad/5DGIIYM/AA/P/AA/datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)

ejercicio1.5 <- function(){
  
  instancias_train <- datos_train[(datos_train[,1] == 1 | datos_train[,1] == 5),]
  instancias_test <- datos_test[(datos_test[,1] == 1 | datos_test[,1] == 5),]

  m_datos_train <- data.matrix(instancias_train)
  m_datos_test <- data.matrix(instancias_test)

  m_datos_train[,2:257] <- 0.5*(1-m_datos_train[,2:257])
  m_datos_test[,2:257] <- 0.5*(1-m_datos_test[,2:257])

  et_train <- m_datos_train[,1]
  et_train[et_train == 5] = -1
  et_test <- m_datos_test[,1]
  et_test[et_test == 5] = -1

  imagenes_train <- list()
  imagenes_test <- list()

  for (i in seq(1, nrow(m_datos_train)))
    imagenes_train <- c(imagenes_train, list(getImagen(m_datos_train[i,2:257])))

  for (i in seq(1, nrow(m_datos_test)))
    imagenes_test <- c(imagenes_test, list(getImagen(m_datos_test[i,2:257])))

  medias_train <- unlist(lapply(imagenes_train, mean))
  simetrias_train <- unlist(lapply(imagenes_train, getSimetria))

  medias_test <- unlist(lapply(imagenes_test, mean))
  simetrias_test <- unlist(lapply(imagenes_test, getSimetria))

  datos <- cbind(medias_train, simetrias_train, 1)

  w_ini <- t(regressLin(datos, et)) #calculamos el vector de pesos dado por la regresión
  w <- ajusta_PLA_MOD(datos, et, 100, w_ini)[[1]] #medimos el numero medio de iteraciones
  
  plot(medias_train, simetrias_train, col = et_train+8, xlab = 'Intensidad promedio', ylab = 'Simetria', main = "Datos Train")
  plotw(w)
  
  
  
  plot(medias_test, simetrias_test, col = et_test+8, xlab = 'Intensidad promedio', ylab = 'Simetria', main = "Datos Test")
  plotw(w)
}

#SOBREAJUSTE

#función recursiva para calcular el valor de un Polinomio de Legendre
LegendreRecursivo <- function(x, grado) {
  if (grado == 0)
    return(c(1,1))
  else if (grado == 1)
    return(c(x,1))
  else
    n = grado-1
    componentes = LegendreRecursivo(x, n)
    mi_valor = (2*n+1)/(n+1)*x*componentes[1]-n/(n+1)*componentes[2]
    return(c(mi_valor, componentes[1]))
}

#función para obtener el valor de un polinomio de Legendre sin tener que usar el vector devuelto por la anterior
Legendre <- function(x, grado) {
  LegendreRecursivo(x, grado)[1]
}

fL <- function(x, coef) {
  Qf = length(coef)
  sum(coef*unlist(lapply(seq(0,Qf-1), Legendre, x = x)))
}

ejercicio2.1 <- function() {
  #damos valores a los parámetros del experimento
  Qf = 20
  N = 50
  sigma = 1
  
  #generamos los coeficientes de la función f
  term_normalizacion = sqrt(sum(1/(2*seq(0,Qf)+1)))
  coef = runif(Qf)/term_normalizacion
  
  #generamos los datos
  X = runif(N, -1, 1)
  ruido = rnorm(N)
  Y = unlist(lapply(X, fL, coef = coef)) + sigma*ruido
  
  Mgrado2 = matrix(X**rep(seq(0,2), each = N), nrow = N)
  Mgrado10 = matrix(X**rep(seq(0,10), each = N), nrow = N)
  
  w2 = regressLin(Mgrado2, Y)
  w10 = regressLin(Mgrado10, Y)
  
  print(t(w2))
  print(t(w10))
}

ejercicio2.2 <- function() {
  Qf = 20
  N = 50
  sigma = 1
  
  E_out_total_2 = 0
  E_out_total_10 = 0
  
  
  for (i in seq(1,100)) {
    #generamos los coeficientes de la función f
    term_normalizacion = sqrt(sum(1/(2*seq(0,Qf)+1)))
    coef = runif(Qf)/term_normalizacion
    
    #generamos los datos
    X = runif(N, -1, 1)
    ruido = rnorm(N)
    Y = unlist(lapply(X, fL, coef = coef)) + sigma*ruido
    
    Mgrado2 = matrix(X**rep(seq(0,2), each = N), nrow = N)
    Mgrado10 = matrix(X**rep(seq(0,10), each = N), nrow = N)
    
    w2 = regressLin(Mgrado2, Y)
    w10 = regressLin(Mgrado10, Y)
    
    #calculamos Eout
    X_out = runif(100, -1,1)
    Y_out = unlist(lapply(X_out, fL, coef = coef))
    
    Mgrado2_out = matrix(X_out**rep(seq(0,2), each = 100), nrow = 100)
    Mgrado10_out = matrix(X_out**rep(seq(0,10), each = 100), nrow = 100)
    
    Y_out_w2 = apply(Mgrado2_out, 1, function(x) w2%*%x)
    Y_out_w10 = apply(Mgrado10_out, 1, function(x) w10%*%x)
    
    E_out_w2 = sum((Y_out - Y_out_w2)^2)/100
    E_out_w10 = sum((Y_out - Y_out_w10)^2)/100
    
    E_out_total_2 = E_out_total_2 + E_out_w2
    E_out_total_10 = E_out_total_10 + E_out_w10
  }
  
  cat("El error medio fuera de la muestra para H2 es: ", E_out_total_2/100, "\n")
  cat("El error medio fuera de la muestra para H10 es: ", E_out_total_10/100, "\n")
}

#REGULARIZACIÓN Y SELECCIÓN DE MODELOS

