library('MASS')
set.seed(41192)
X11()
#Funciones externas
simula_unif <- function (N, dim, rango) {
  datos <- matrix(runif(N*dim, rango[1], rango[2]), ncol = dim, nrow = N)
  datos
}

simula_gaus <- function(N, dim, sigma) {
  datos <- matrix(rnorm(N*dim, 0, sigma), ncol = dim, nrow = N)
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
@dibujar: booleano controlando si pintamos la convergencia del método o no
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
    valores_f = c(valores_f, f_ant) #vamos almacenando los valores de f que se alcanzan durante el proceso
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
    
    w[1] = w[1] - eta*gradf(w)[1] #modificamos la primera componente del vector de pesos en base a la primera coordenada del gradiente
    w[2] = w[2] - eta*gradf(w)[2] #modificamos la segunda componente del vector de pesos en base a la segunda coordenada del gradiente
    
    #condición de parada
    if (f(w) < tol || n_iters == max_iter || d(w_ant, w) <= tol || abs(f(w) - f_ant) <= tol) break
  }
  
  cat("Iteraciones empleadas: ", n_iters, "\n")
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
Newton <- function(f, gradf, Hf, eta, w0, tol, max_iter = 1000000, dibujar = FALSE){
  w = w0
  n_iters = 1
  valores_f = NULL
  
  repeat {
    w_ant = w 
    f_ant = f(w)
    
    valores_f = c(valores_f, f_ant) #vamos almacenando los valores alcanzados de la f
    w = w-t(eta*ginv(Hf(w))%*%gradf(w)) #actualizamos el vector de pesos
    n_iters = n_iters + 1
    
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
  n_etapas = 0
  
  repeat{
    w_ant = w
    n_etapas = n_etapas + 1
    
    for (i in sample(N)) {
      gt = (-et[i]*datos[i,])/(1+exp(et[i]*w%*%datos[i,])) #calculamos el vector de incremento
      w = w - eta*gt #actualizamos el vector de pesos
    }
    
    #condición de parada
    if (d(w, w_ant) < tol)
      break
  }
  
  list(w, n_etapas)
}

ejercicio1.4 <- function(){
  err_out_medio = 0
  n_etapas_medio = 0
  n_experimentos = 100
  
  for (i in seq(n_experimentos)) {
    #generamos los datos de entrenamiento y test
    datos <- simula_unif(100, 2, c(-1,1))
    datos_out <- simula_unif(100,2,c(-1,1))
    
    #calculamos las etiquetas para dichos datos en base a una recta generada de forma aleatoria
    recta <- simula_recta(c(-1,1))
    ets <- mapply(f0, recta[2], recta[1], datos[,1], datos[,2])
    ets_out <- mapply(f0, recta[2], recta[1], datos_out[,1], datos_out[,2])
    
    #obtenemos el vector de pesos dado por regresión logística y calculamos el error en los datos de test
    result = RL(datos, ets, 0.01, c(0,0), 0.01)
    w = result[[1]]
    ets_reg_out <- apply(datos_out, 1, h, w = w) #obtenemos las etiquetas que el vector dado por la regresion le da a los datos fuera de la muestra
    ets_reg_out[which(ets_reg_out == -1)] = 0
    
    n_etapas_medio = n_etapas_medio + result[[2]]
    err_out_medio = err_out_medio + length(which(ets_reg_out != ets_out))
  }
  
  cat("El número medio de etapas que consume RL antes de converger es: ", n_etapas_medio/n_experimentos, "\n")
  cat("El error medio fuera de la muestra para RL es: ", err_out_medio/n_experimentos, "\n")
}

#Ejercicio 1.5
#datos_train <- read.table("~/AA/datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
#datos_test <- read.table("~/AA/datos/zip.test", quote="\"", comment.char="", stringsAsFactors=FALSE)
#datos_test <- read.table("/media/griger/Datos/Documentos/Facultad/5DGIIYM/AA/P/AA/datos/zip.test", quote="\"", comment.char="", stringsAsFactors=FALSE)
#datos_train <- read.table("/media/griger/Datos/Documentos/Facultad/5DGIIYM/AA/P/AA/datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
datos_test <- read.table("datos/zip.test", quote="\"", comment.char="", stringsAsFactors=FALSE)
datos_train <- read.table("datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)

ejercicio1.5 <- function(){
  #seleccionamos los 1 y 5 de ambos conjuntos de datos
  instancias_train <- datos_train[(datos_train[,1] == 1 | datos_train[,1] == 5),]
  instancias_test <- datos_test[(datos_test[,1] == 1 | datos_test[,1] == 5),]
  
  m_datos_train <- data.matrix(instancias_train)
  m_datos_test <- data.matrix(instancias_test)
  
  #normalizamos los datos
  m_datos_train[,2:257] <- 0.5*(1-m_datos_train[,2:257])
  m_datos_test[,2:257] <- 0.5*(1-m_datos_test[,2:257])
  
  #modificamos las etiquetas para que funcionen correctamente con regresión
  et_train <- m_datos_train[,1]
  et_train[et_train == 5] = -1
  et_test <- m_datos_test[,1]
  et_test[et_test == 5] = -1

  imagenes_train <- list()
  imagenes_test <- list()
  
  #creamos matrices a partir de las características de cada dato
  for (i in seq(1, nrow(m_datos_train)))
    imagenes_train <- c(imagenes_train, list(getImagen(m_datos_train[i,2:257])))

  for (i in seq(1, nrow(m_datos_test)))
    imagenes_test <- c(imagenes_test, list(getImagen(m_datos_test[i,2:257])))
  
  #calculamos medias y simetrías
  medias_train <- unlist(lapply(imagenes_train, mean))
  simetrias_train <- unlist(lapply(imagenes_train, getSimetria))

  medias_test <- unlist(lapply(imagenes_test, mean))
  simetrias_test <- unlist(lapply(imagenes_test, getSimetria))

  datos <- cbind(medias_train, simetrias_train, 1)
  datos_test <- cbind(medias_test, simetrias_test, 1)

  w_ini <- t(regressLin(datos, et_train)) #calculamos el vector de pesos dado por la regresión
  w <- ajusta_PLA_MOD(datos, et_train, 100, w_ini)[[1]] #empleamos el PLA_Pocket partiendo del vector de pesos que hemos obtenido
  
  Y_wlin_train = apply(datos, 1, h, w = w) #etiquetas dadas por el vector de pesos
  Y_wlin_test = apply(datos_test, 1, h, w = w) #etiquetas dadas por el vector de pesos
  
  E_in = length(which(et_train != Y_wlin_train))
  E_test = length(which(et_test != Y_wlin_test))
  cat("El error dentro de la muestra es, E_in: ", E_in, "\n")
  cat("El error fuera  de la muestra es, E_test: ", E_test, "\n")
  
  par(mfrow = c(1,2))
  plot(medias_train, simetrias_train, col = et_train+5, xlab = 'Intensidad promedio', ylab = 'Simetria', main = "Datos Train")
  plotw(w)
  
  plot(medias_test, simetrias_test, col = et_test+5, xlab = 'Intensidad promedio', ylab = 'Simetria', main = "Datos Test")
  plotw(w)
  
  
  N_train = length(medias_train) #numero de datos de entrenamiento que tenemos
  N_test = length(medias_test) #numero de dato de test que tenemos 
  
  #Calculamos las cotas de Eout empleando E_in y empleando E_test.
  cat("Usando E_in enemos que Eout <= ", E_in + sqrt((8/N_train)*log((4*(2*N_train^3+1))/0.05)), "\n" )
  
  eps = sqrt(log(0.05/2)/(-2*N_test))
  cat("Usando E_test tenemos que |E_test - E_out| <=", eps, "\n")
}

#SOBREAJUSTE

#función recursiva para calcular el valor de un Polinomio de Legendre
'
@x: el valor donde queremos evaluar el polinomio
@grado: el grado del polinomio de Legendre a evaluar
'
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

#La función objetivo para esta parte de la práctica
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
  
  #Obtenemos los datos a ajustar obteniendo las distintas potencias de los valores anteriormente obtenidos
  Mgrado2 = matrix(X**rep(seq(0,2), each = N), nrow = N)
  Mgrado10 = matrix(X**rep(seq(0,10), each = N), nrow = N)
  
  #Obtenemos los vector de pesos con regresión lineal
  w2 = regressLin(Mgrado2, Y)
  w10 = regressLin(Mgrado10, Y)
  
  cat("g2 ", t(w2), "\n")
  cat("g10", t(w10), "\n")
}

ejercicio2.2 <- function() {
  Qf = 20
  N = 50
  sigma = 1
  
  E_out_total_2 = 0
  E_out_total_10 = 0
  
  
  for (i in seq(1,200)) {
    #generamos los coeficientes de la función f
    term_normalizacion = sqrt(sum(1/(2*seq(0,Qf)+1)))
    coef = runif(Qf)/term_normalizacion
    
    #generamos los datos
    X = runif(N, -1, 1)
    ruido = rnorm(N)
    Y_ruido = unlist(lapply(X, fL, coef = coef)) + sigma*ruido
    
    Mgrado2 = matrix(X**rep(seq(0,2), each = N), nrow = N)
    Mgrado10 = matrix(X**rep(seq(0,10), each = N), nrow = N)
    
    w2 = regressLin(Mgrado2, Y_ruido)
    w10 = regressLin(Mgrado10, Y_ruido)
    
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
  
  cat("El error medio fuera de la muestra para H2 es: ", E_out_total_2/200, "\n")
  cat("El error medio fuera de la muestra para H10 es: ", E_out_total_10/200, "\n")
}

#REGULARIZACIÓN Y SELECCIÓN DE MODELOS
'
Función que calcula una vector de pesos usando regresión lineal con weight decay
@Z: matriz de datos
@label: etiquetas para los datos
@lambda: factor de regularización
'
regressLinWeightDecay <- function(Z, label, lambda) {
  ncol = ncol(Z)
  ginv(t(Z)%*%Z + lambda*diag(ncol))%*%t(Z)%*%label
}

ejercicio3.1 <- function(term_norm = 0.05) {
  Nexp = 10^3
  d = 3
  
  Ns = seq(15,115,10)+d
  results = lapply(Ns+1, function(i) rep(0,i))
  wf = rnorm(d+1) #obtenemos los coeficientes del polinomio a ajustar
  
  varE1Ns = rep(0, length(Ns))
  varEcvNs = rep(0,length(Ns))
  
  for (N in Ns) {
    e1s = rep(0, Nexp)
    e2s = rep(0, Nexp)
    Ecvs = rep(0, Nexp)
    
    for (i in seq(1,Nexp)) {
      #generamos una muestra de datos junto con sus etiquetas
      X = simula_gaus(N, d, 1)
      X = cbind(X, 1)
      Y = apply(X, 1, function(x) wf%*%x)
      Y_ruido = apply(X, 1, function(x) wf%*%x + 0.5*rnorm(1))
      
      idx = seq(1,N)
      
      #calculamos los ei quitando un dato para acada uno y aplicando regresión lineal con weight decay
      Eis <- sapply(idx, function(i) {
        wlin = regressLinWeightDecay(X[-i,], Y_ruido[-i], term_norm/N)
        ei = (t(wlin)%*%X[i,] - Y_ruido[i])^2
      })
      
      #almacenamos los e1, los e2 y los Ecv de cada experimento para luego calcular su media y su varianza
      e1s[i] = Eis[1]
      e2s[i] = Eis[2]
      Ecvs[i] = mean(Eis)
      
      #almacenamos los resultados en promedio de los experimentos por si queremos mostrarlos
      result = c(Eis, mean(Eis))
      results[[N%/%10]] <- results[[N%/%10]] + result/Nexp
    }
    
    varE1Ns[N%/%10] = var(e1s)
    varEcvNs[N%/%10] = var(Ecvs)
    
    cat("Para N: ", N, "e1 medio es ", mean(e1s), " y su varianza ", varE1Ns[N%/%10], " \n")
    cat("Para N: ", N, "e2 medio es ", mean(e2s), " y su varianza ", var(e2s), " \n")
    cat("Para N: ", N, "e1 medio es ", mean(Ecvs), " y su varianza ", varEcvNs[N%/%10], " \n\n\n")
  }
  
  cat("Los Neff para los distintos N con los que trabajamos:\n ", varE1Ns/varEcvNs, "\n")
  cat("La media de los anteriores valores es: ", mean(varE1Ns/varEcvNs), "\n")
  
  #dibujar número efectivo
  par(mfrow = c(1,1))
  plot(Ns, 100*Ns/(varE1Ns/varEcvNs))
}


print("MODELOS LINEALES")
print("Ejercicio 1. Gradiente Descendente")
print("Apartado a")
gradDesc(E, gradE, 0.1, c(1,1), 10**(-14))
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Apartado b")
gradDesc(f, gradf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
gradDesc(f, gradf, 0.1, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
gradDesc(f, gradf, 0.01, c(0.1,0.1), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(-0.5,-0.5), 10**(-14), max_iter = 50, dibujar = FALSE)
gradDesc(f, gradf, 0.01, c(-1,-1), 10**(-14), max_iter = 50, dibujar = FALSE)
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio 2. Coordenada descendente")
coordDesc(E, gradE, 0.1, c(1,1), 10**(-14), 15)
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio3. Método de Newton")
print("Gráficas y datos con Gradiente Descendente")
par(mfrow=c(2,2))
gradDesc(f, gradf, 0.01, c(0.1,0.1), 10**(-14), max_iter = 50, dibujar = TRUE)
gradDesc(f, gradf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
gradDesc(f, gradf, 0.01, c(-0.5,-0.5), 10**(-14), max_iter = 50, dibujar = TRUE)
gradDesc(f, gradf, 0.01, c(-1,-1), 10**(-14), max_iter = 50, dibujar = TRUE)
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Gráficas y datos con el método de Newton")
Newton(f, gradf, Hf, 0.01, c(0.1,0.1), 10**(-14), max_iter = 50, dibujar = TRUE)
Newton(f, gradf, Hf, 0.01, c(1,1), 10**(-14), max_iter = 50, dibujar = TRUE)
Newton(f, gradf, Hf, 0.01, c(-0.5,-0.5), 10**(-14), max_iter = 50, dibujar = TRUE)
Newton(f, gradf, Hf, 0.01, c(-1,-1), 10**(-14), max_iter = 50, dibujar = TRUE)

print("Ejercicio 4. Regresión Logística")
ejercicio1.4()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio 5. Clasificación de dígitos")
ejercicio1.5()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("SOBREAJUSTE")
print("Ejercicio 1. Sobreajuste")
ejercicio2.1()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio 2.")
ejercicio2.2()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("REGULARIZACIÓN Y SELECCIÓN DE MODELOS")
print("Ejercicio 1. Ejecución con lamda 0.05/N")
ejercicio3.1()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 1. Ejecución con lamda 2.5/N")
ejercicio3.1(2.5)
print ("Pulsar ENTER para salir.")
invisible(readLines("stdin", n=1))


