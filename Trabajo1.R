#Trabajo 1 por Gustavo Rivas Gervilla
#3.2
#Ejercicio 1
library(MASS)
set.seed(41192)
X11()

simula_unif <- function(N, dim, rango) {
  lista <- list() #generamos una lista vacia
  
  #vamos añadiendo componentes a dicha lista que son arrays de dim componente muestreando datos
  #de una uniforme con la función runif a la que le pasamos el número de datos a generar y el rango.
  for (i in seq(1:N))
    lista <- c(lista, list(array(runif(dim, rango[1], rango[2]), dim)))

  lista
}

simula_gaus <- function(N, dim, sigma) {
  lista <- list()

  for (i in seq(1:N))
    lista <- c(lista, list(array(rnorm(dim, 0, sigma), dim)))

  lista
}

#Ejercicio 3
#Hago una nueva funcion para que sea cómoda la representación de los datos.
simula_unif <- function (N, dim, rango) {
  #generamos tantos datos como necesitemos de una uniforme y los organizamos en una matriz
  #le damos las filas y columnas que queremos que tenga con ncol y nrow
  #el primer parametro son los datos a introducir en la matriz, la muestra uniforme del tamano indicado
  datos <- matrix(runif(N*dim, rango[1], rango[2]), ncol = dim, nrow = N)

  datos
}

simula_gaus <- function(N, dim, sigma) {
  datos <- matrix(rnorm(N*dim, 0, sigma), ncol = dim, nrow = N)
  
  datos
}

#Ejercicio 3
ejercicio3 <- function() {
  datos <- simula_unif(50, 2, c(-50,50)) #generamos los datos con dos dimensiones
  plot(datos[,1], datos[,2], main = "Uniforme", xlab = "X", ylab = "Y") #dibujamos los datos, cada dimension, cada columna, en un eje.
}

#Ejercicio 4
ejercicio4 <- function() {
  datos <- simula_gaus(50, 2, c(5,7)) #generamos los datos con dos dimensiones
  plot(datos[,1], datos[,2], main = "Gaussiana", xlab = "X", ylab = "Y") #dibujamos los datos, cada dimension, cada columna, en un eje.
}

#Ejercicio 5
simula_recta <- function(rango = c(-50,50)) {
  coordenadas = simula_unif(2,2,rango) #generamos dos ptos en el cuadrado rangoxrango
  a = (coordenadas[2,2]-coordenadas[1,2])/(coordenadas[2,1]-coordenadas[1,1])#la pendiente (m)
  b = -a*coordenadas[1,1] + coordenadas[1,2] #el factor independiente en la ecuacion y = mx+b

  c(a,b)
}

#Ejercicio 6
f0 <- function(a, b, x, y) {sign(y-b*x-a)} #funcion que nos da las etiquetas para el ejercicio 6

muestra = simula_unif(50, 2, c(-50,50)) #generamos una muestra para emplearla en los sucesivos ejercicios
muestra_hom = cbind(muestra, 1) #anadimos la coordenada homogenea para los ejercicios de PLA y sucesivos

ejercicio6 <- function() {
  recta = simula_recta() #obtenemos una recta
  et <- mapply(f0, recta[2], recta[1], muestra[,1], muestra[,2]) #evaluamos f0 en cada dato, en cada fila de la matriz
  plot(muestra, main = "Ejercicio 6", col=et+2, xlim = c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y') #mostramos los datos
  abline(a = recta[2], b = recta[1]) #anadimos la recta al plot actual
}

#Ejercicio 7

#definimos las funciones que nos daran las etiquetas
f1 <- function(x,y) {sign((x-10)^2 + (y-20)^2 - 400)}
f2 <- function(x,y) {sign(0.5*(x+10)^2 + (y-20)^2 - 400)}
f3 <- function(x,y) {sign(0.5*(x-10)^2 - (y+20)^2 - 400)}
f4 <- function(x,y) {sign(y - 20*x^2 - 5*x + 3)}

#funcion para dibujar una funcion implicita
mostrar_funcion <- function(f) {
  #hacemos un submuestreo de la region donde pintaremos la funcion
  x <- seq(-50,50,length = 1000)
  y <- seq(-50,50,length = 1000)
  #calculamos el valor de la funcion en esas muestras
  z <- outer(x,y,f)
  
  #dibujamos la funcion con los datos indicados
  contour(x,y,z,levels=0, drawlabels = FALSE)
  
}

ejercicio7 <- function() {
  #Hacemos lo mismo que en el ejercicio 6 pero con las nuevas funciones
  et1 <- mapply(f1, muestra[,1], muestra[,2])
  et2 <- mapply(f2, muestra[,1], muestra[,2])
  et3 <- mapply(f3, muestra[,1], muestra[,2])
  et4 <- mapply(f4, muestra[,1], muestra[,2])
  
  #Mostramos los datos junto con sus etiquetas
  #print(cbind(muestra,et1))
  #print(cbind(muestra, et2))
  #print(cbind(muestra,et3))
  #print(cbind(muestra, et4))

  par(mfrow = c(2,2)) #dividimos la region de dibujo en 4
  
  
  plot(muestra, col = et1+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE) #para que contour no borre los puntos dibujados
  mostrar_funcion(f1)
  plot(muestra, col = et2+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE)
  mostrar_funcion(f2)
  plot(muestra, col = et3+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE)
  mostrar_funcion(f3)
  plot(muestra, col = et4+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE)
  mostrar_funcion(f4)

}

#Ejercicio 8
etiquetas_ruidosas = 0

introducirRuido <- function(etiquetas_originales) {
  et_ruido <- etiquetas_originales
  
  #Calculamos el numero de etiquetas positivas y negativas
  n_positivos <- length(which(et_ruido == 1))
  n_negativos <- length(et_ruido) - n_positivos
  
  #Hacemos un muestreo aleatorio de tamaño el 10% de las etiquetas correspondientes.
  idx_positivos <- sample(which(et_ruido == 1), ceiling(n_positivos*0.1)) #positivos
  idx_negativos <- sample(which(et_ruido == -1), ceiling(n_negativos*0.1)) #negativos
  
  #Con los indices muestreados aleatoriamente cambiamos los valores de esas posiciones
  et_ruido[idx_positivos] = -1
  et_ruido[idx_negativos] = 1
  
  #devolvemos las etiquetas
  et_ruido
}

ejercicio8 <- function() {
  
  par(mfrow = c(2,1))
  
  #Mostramos tanto el etiquetado original como el ruidoso junto con la recta
  recta = simula_recta()
  et <- mapply(f0, recta[2], recta[1], muestra[,1], muestra[,2])
  plot(muestra, col=et+2, xlim = c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  abline(a = recta[2], b = recta[1])
  
  #Introducimos las etiquetas en una variables global para futuros usos por eso usamos el <<-
  etiquetas_ruidosas <<- introducirRuido(et) 

  plot(muestra, col=etiquetas_ruidosas+2, xlim = c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  abline(a = recta[2], b = recta[1])
  
  Sys.sleep(3) #paramos la ejecucion 3 segundos
  
  #Mostramos el ruido en las etiquetas dadas por las funciones no lineales.
  par(mfrow = c(2,2)) #dividimos la region en 4
  
  et1 <- introducirRuido(mapply(f1, muestra[,1], muestra[,2]))
  et2 <- introducirRuido(mapply(f2, muestra[,1], muestra[,2]))
  et3 <- introducirRuido(mapply(f3, muestra[,1], muestra[,2]))
  et4 <- introducirRuido(mapply(f4, muestra[,1], muestra[,2]))
  
  plot(muestra, col = et1+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE)
  mostrar_funcion(f1)
  plot(muestra, col = et2+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE) 
  mostrar_funcion(f2)
  plot(muestra, col = et3+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE) 
  mostrar_funcion(f3)
  plot(muestra, col = et4+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  par(new = TRUE) 
  mostrar_funcion(f4)
}



#3.3 PLA

#funcion h del PLA
h <- function(w, dato) {
  sign(w%*%dato) #signo del producto escalar entre w y el dato
}

#funcion que dibujar la recta dada por los pesos de w, despejando de la ecuacion xw1+yw2+w3 = 0
plotw <- function(w, color = 'black') {
  if (w[2] != 0) #si podemos despejar el termino que va con y
    abline(a = -w[1]/w[2], b = -w[3]/w[2], col = color)
  else if (w[1] != 0) #si lo que tenemos es una recta vertical
    abline(v = -w[3]/w[1], col = color)
  else
    print("No hay linea.")
}

ajustaPLA <- function(datos, label, max_iter, vini, plot = FALSE) {
  w <- vini
  num_datos <- nrow(datos)
  encontrado = FALSE
  iter = 1
  
  #mientras no superemos el max_iter y no hayamos encontrado la recta que separa los datos
  while (!encontrado && iter < max_iter) {
    encontrado <- TRUE
    for (i in seq_len(num_datos))
      #si el w actual no etiqueta bien al dato lo actualizamos
      if (h(w, datos[i,]) != label[i]) {
        w <- w + label[i]*datos[i,]
        encontrado <- FALSE
      }
    
    #En caso de que queramos dibujar las soluciones que vayamos generando.
    if (plot) {
      plot.new()#reiniciamos el area de dibujo
      plot(datos, col = label+2, xlim=c(-50,50), ylim = c(-50,50)) #pintamos los datos
      plotw(w) #pintamos la recta dada por los coeficientes
      Sys.sleep(0.5) #esperamos medio segundo antes de volver a dibujar
    }

    iter = iter + 1
  }
  
  #devolvemos tanto los pesos obtenidos como el numero de iteraciones empleados
  list(w,iter)
}

ejercicio3.2 <- function() {
  iteraciones = 0

  vini <- c(0,0,0)
  recta <- simula_recta() #obtenemos la recta que dara las etiquetas
  etiquetas <- sign(muestra_hom[,2]-recta[1]*muestra_hom[,1]-recta[2]) #obtenemos las etiquetas
  
  cat("Numero de it con el cte 0: ", ajustaPLA(muestra_hom, etiquetas, 1000, vini)[[2]], "\n")

  for (i in seq_len(10)) {
    vini <- runif(3) #generamos un vector de numeros aleatorios
    iteraciones = iteraciones + ajustaPLA(muestra_hom, etiquetas, 1000, vini)[[2]]
  }

  cat("Numero medio de iteraciones con vectores aleatorios: ", iteraciones/10, "\n")
}

ejercicio3.3 <- function() {
  #obtenemos los vectores de pesos
 	w10 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 10, c(0,0,0))[[1]]
	w100 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 100, c(0,0,0))[[1]]
	w1000 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 1000, c(0,0,0))[[1]]
	
	#obtenemos las etiquetas
	et10 <- apply(muestra_hom, 1, h, w = w10)
	et100 <- apply(muestra_hom, 1, h, w = w100)
	et1000 <- apply(muestra_hom, 1, h, w = w1000)
	
	#calculamos los errores
	err10 <- length(which(etiquetas_ruidosas != et10))
	err100 <- length(which(etiquetas_ruidosas != et100))
	err1000 <- length(which(etiquetas_ruidosas != et1000))
	
	cat("Con 10 iteraciones max. obtenemos el w = (", w10, ") con el cual obtenemos", err10, " errores.\n")
	cat("Con 100 iteraciones max. obtenemos el w = (", w100, ") con el cual obtenemos", err100, " errores.\n")
	cat("Con 1000 iteraciones max. obtenemos el w = (", w1000, ") con el cual obtenemos", err1000, " errores.\n")
	

}

ejercicio3.4 <- function() {
  et <-  introducirRuido(mapply(f1, muestra[,1], muestra[,2])) #obtenemos las etiquetas originales
  
  #obtenemos los vectores de pesos
  w10 <- ajustaPLA(muestra_hom, et, 10, c(0,0,0))[[1]]
  w100 <- ajustaPLA(muestra_hom, et, 100, c(0,0,0))[[1]]
  w1000 <- ajustaPLA(muestra_hom, et, 1000, c(0,0,0))[[1]]
  
  #obtenemos las etiquetas
  et10 <- apply(muestra_hom, 1, h, w = w10)
  et100 <- apply(muestra_hom, 1, h, w = w100)
  et1000 <- apply(muestra_hom, 1, h, w = w1000)
  
  #calculamos los errores
  err10 <- length(which(et != et10))
  err100 <- length(which(et != et100))
  err1000 <- length(which(et != et1000))
  
  cat("Con 10 iteraciones max. obtenemos el w = (", w10, ") con el cual obtenemos", err10, " errores.\n")
  cat("Con 100 iteraciones max. obtenemos el w = (", w100, ") con el cual obtenemos", err100, " errores.\n")
  cat("Con 1000 iteraciones max. obtenemos el w = (", w1000, ") con el cual obtenemos", err1000, " errores.\n")
}

ejercicio3.5 <- function() {
  par(mfrow = c(1,1))
  print("10 iter_max")
  w10 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 10, c(0,0,0), plot = TRUE)[[1]]
  print("100 iter_max")
  w100 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 100, c(0,0,0), plot = TRUE)[[1]]
  print("1000 iter_max")
  w1000 <- ajustaPLA(muestra_hom, etiquetas_ruidosas, 1000, c(0,0,0), plot = TRUE)[[1]]
  
  et10 <- apply(muestra_hom, 1, h, w = w10)
  et100 <- apply(muestra_hom, 1, h, w = w100)
  et1000 <- apply(muestra_hom, 1, h, w = w1000)
  
  err10 <- length(which(etiquetas_ruidosas != et10))
  err100 <- length(which(etiquetas_ruidosas != et100))
  err1000 <- length(which(etiquetas_ruidosas != et1000))
  
  cat("Con 10 iteraciones max. obtenemos el w = (", w10, ") con el cual obtenemos", err10, " errores.\n")
  cat("Con 100 iteraciones max. obtenemos el w = (", w100, ") con el cual obtenemos", err100, " errores.\n")
  cat("Con 1000 iteraciones max. obtenemos el w = (", w1000, ") con el cual obtenemos", err1000, " errores.\n")
  
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

ejercicio3.6 <- function() {
  #cargamos las etiquetas
  et1 <- mapply(f1, muestra[,1], muestra[,2])
  et2 <- mapply(f2, muestra[,1], muestra[,2])
  et3 <- mapply(f3, muestra[,1], muestra[,2])
  et4 <- mapply(f4, muestra[,1], muestra[,2])
  
  #calculamos los w dados por ambas versiones del PLA
  w1PLA <- ajustaPLA(muestra_hom, et1, 100, c(0,0,0))[[1]]
  w2PLA <- ajustaPLA(muestra_hom, et2, 100, c(0,0,0))[[1]]
  w3PLA <- ajustaPLA(muestra_hom, et3, 100, c(0,0,0))[[1]]
  w4PLA <- ajustaPLA(muestra_hom, et4, 100, c(0,0,0))[[1]]
  
  w1PLA_MOD <- ajusta_PLA_MOD(muestra_hom, et1, 100, c(0,0,0))[[1]]
  w2PLA_MOD <- ajusta_PLA_MOD(muestra_hom, et2, 100, c(0,0,0))[[1]]
  w3PLA_MOD <- ajusta_PLA_MOD(muestra_hom, et3, 100, c(0,0,0))[[1]]
  w4PLA_MOD <- ajusta_PLA_MOD(muestra_hom, et4, 100, c(0,0,0))[[1]]
  
  #calculamos las etiquetas dadas para esos datos
  etw1PLA <- apply(muestra_hom, 1, h, w = w1PLA)
  etw2PLA <- apply(muestra_hom, 1, h, w = w2PLA)
  etw3PLA <- apply(muestra_hom, 1, h, w = w3PLA)
  etw4PLA <- apply(muestra_hom, 1, h, w = w4PLA)
  
  etw1PLA_MOD <- apply(muestra_hom, 1, h, w = w1PLA_MOD)
  etw2PLA_MOD <- apply(muestra_hom, 1, h, w = w2PLA_MOD)
  etw3PLA_MOD <- apply(muestra_hom, 1, h, w = w3PLA_MOD)
  etw4PLA_MOD <- apply(muestra_hom, 1, h, w = w4PLA_MOD)
  
  #calculamos los errores
  errw1PLA <- length(which(et1 != etw1PLA))
  errw2PLA <- length(which(et2 != etw2PLA))
  errw3PLA <- length(which(et3 != etw3PLA))
  errw4PLA <- length(which(et4 != etw4PLA))
  
  errw1PLA_MOD <- length(which(et1 != etw1PLA_MOD))
  errw2PLA_MOD <- length(which(et2 != etw2PLA_MOD))
  errw3PLA_MOD <- length(which(et3 != etw3PLA_MOD))
  errw4PLA_MOD <- length(which(et4 != etw4PLA_MOD))
  
  #mostramos los resultados
  cat("Sobre las etiquetas de la primera funcion. Errores de PLA: ", errw1PLA, "Errores de PLA_MOD: ", errw1PLA_MOD, "\n")
  cat("Sobre las etiquetas de la segunda funcion. Errores de PLA: ", errw2PLA, "Errores de PLA_MOD: ", errw2PLA_MOD, "\n")
  cat("Sobre las etiquetas de la tercera funcion. Errores de PLA: ", errw3PLA, "Errores de PLA_MOD: ", errw3PLA_MOD, "\n")
  cat("Sobre las etiquetas de la cuarta funcion. Errores de PLA: ", errw4PLA, "Errores de PLA_MOD: ", errw4PLA_MOD, "\n")
  
  #pintamos el resultado
  par(mfrow = c(2,2))
  
  plot(muestra, col = et1+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  plotw(w1PLA)
  plotw(w1PLA_MOD, 'red')
  plot(muestra, col = et2+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  plotw(w2PLA)
  plotw(w2PLA_MOD, 'red')
  plot(muestra, col = et3+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  plotw(w3PLA)
  plotw(w3PLA_MOD, 'red')
  plot(muestra, col = et4+2, xlim=c(-50,50), ylim = c(-50,50), xlab = 'X', ylab = 'Y')
  plotw(w4PLA)
  plotw(w4PLA_MOD, 'red')
  
}

#3.4 Regresión lineal

#funcion que crea una matriz 16x16 a partir de una fila con los 16x16 pixeles
getImagen <- function(datos) {
  matrix(datos, ncol = 16) #le pasamos los datos y las columnas que tiene que tener la matriz que formemos con ellos
}

#funcion que calcula la simetria de una imagen
getSimetria <- function(m) {
  -2*sum(abs(m[,seq(1,8)]-m[,seq(16,9)]))
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

digitos_entrenamiento <- read.table("datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
#getSrcDirectory(function(x) {x}) -> base
#path <- paste(base,"/datos/zip.train", sep = "")
#digitos_entrenamiento <- read.table(path, quote="\"", comment.char="", stringsAsFactors=FALSE)
#digitos_entrenamiento <- read.table("./datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
#digitos_entrenamiento <- read.table("/media/usuario/Datos/Documentos/Facultad/5DGIIYM/AA/P/DigitosZip/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
#digitos_entrenamiento <- read.table("C:/Users/Griger/Desktop/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)


ejercicio4.2 <- function() {
  #tomamos solo las instancias correspondientes a 1 y 5
  instancias <- digitos_entrenamiento[(digitos_entrenamiento[,1] == 1 | digitos_entrenamiento[,1] == 5),]
  #creamos una matriz con dichos datos
  m_datos <- data.matrix(instancias)
  #normalizamos los datos
  m_datos[,2:257] <- 0.5*(1-m_datos[,2:257])
  #obtenemos solo las etiquetas de los datos
  et <- m_datos[,1]
  
  #Guardamos cada imagen como una matriz 16x16 y creamos una lista con ellas
  imagenes <- list()
  
  for (i in seq(1, nrow(m_datos)))
    imagenes <- c(imagenes, list(getImagen(m_datos[i,2:257])))
  
  #creamos una array con las medias y simetrias de las imagenes
  medias <- unlist(lapply(imagenes, mean))
  simetrias <- unlist(lapply(imagenes, getSimetria))
  #ajustamos una recta a la una funcion que para cada media de una imagen devuelve la simetria de dicha imagen
  #fabricamos entonces la matriz de datos X que sera el resultado de homogeneizar las medias
  datos <- matrix(medias, ncol = 1)
  datos <- cbind(datos, 1)
  
  par(mfrow = c(1,1))
  
  
  wlin <- t(regressLin(datos, simetrias))
  
  #mostramos el resultado
  plot(medias, simetrias, col = et+8, xlab = 'Intensidad promedio', ylab = 'Simetria')
  cat("wlin: ", wlin, "\n")
  abline(a = wlin[,2], b = wlin[,1])
}


ejercicio4.7 <- function() {
  #apartado a
  err_in = 0;
  
  for (i in seq(1,1000)) {
    datos <- simula_unif(100, 2, c(-10,10)) #generamos datos
    recta <- simula_recta(rango = c(-10,10)) #generamos recta
    et <- mapply(f0, recta[2], recta[1], datos[,1], datos[,2]) #las etiquetas dadas por la recta
  
    datos_hom <- cbind(datos, 1) #homgeneizamos datos
    w <- t(regressLin(datos_hom, et)) #obtenemos vector de pesos
    et_reg <- apply(datos_hom, 1, h, w = w) #etiquetas dadas por el vector de pesos
    
   
    err_in = err_in + 0.001*length(which(et_reg != et)) #medimos el error
    
  }
  
  print("Apartado a")
  cat("Numero medio de E_in:", err_in, "\n")
  cat("Porcentaje medio de E_in:", err_in, "%\n")
  
  #apartado b
  
  err_out = 0;
  
  for (i in seq(1,1000)) {
    datos <- simula_unif(100, 2, c(-10,10)) #generamos los datos de entrenamiento
    datos_out <- simula_unif(1000, 2, c(-10,10)) #generamos los datos de test
    recta <- simula_recta(rango = c(-10,10)) #generamos recta
    et <- mapply(f0, recta[2], recta[1], datos[,1], datos[,2]) #etiquetas de la recta para los datos de entrenamiento
    et_out <- mapply(f0, recta[2], recta[1], datos_out[,1], datos_out[,2])  #etiquetas de la recta para los datos de test
    
    #homogeneizamos datos
    datos_hom <- cbind(datos, 1) 
    datos_out_hom <- cbind(datos_out, 1) 
    
    w <- t(regressLin(datos_hom, et)) #calculamos el vector de pesos
    et_reg_out <- apply(datos_out_hom, 1, h, w = w) #etiquetas dadas por el vector de pesos a los datos de test
    
    err_out = err_out + 0.001*length(which(et_reg_out != et_out)) #calculamos el error medio
    
  }
  print("Apartado b")
  cat("Numero medio de E_out: ", err_out, "\n")
  cat("Numero medio de porcentaje de E_out: ", err_out/1000*100, "%\n")
  #apartado c
  iteraciones = 0
  
  for (i in seq(1,1000)) {
    datos <- simula_unif(10, 2, c(-10,10)) #gerenamos datos
    datos_hom <- cbind(datos, 1) #homogeneizamos datos
    recta <- simula_recta(rango = c(-10,10)) #calculamos recta
    et <- mapply(f0, recta[2], recta[1], datos[,1], datos[,2]) #obtenemos las etiquetas dadas por la muestra
    
    w_ini <- t(regressLin(datos_hom, et)) #calculamos el vector de pesos dado por la regresión
    
    iteraciones = iteraciones + 0.001*ajustaPLA(datos_hom, et, 1000, w_ini)[[2]] #medimos el numero medio de iteraciones
  }
  
  print("Apartado c")
  cat("Numero medio de iteraciones PLA: ", iteraciones, "\n")
  
  
}

fi <- function(dato) {
  c(1, dato[1], dato[2], dato[1]*dato[2], dato[1]^2, dato[2]^2)
}

f5 <- function(dato) {
  sign(dato[1]*dato[1]+dato[2]*dato[2]-25)
  
}


ejercicio4.8 <- function() {
  #apartado a
  err_in = 0
  for (i in seq(1,1000)) {
    datos <- simula_unif(1000, 2, c(-10,10)) #generamos datos
    et <- introducirRuido(apply(datos, 1, f5)) #introducimos ruido
    
    datos_hom <- cbind(datos, 1) #homogeneizamos datos
    w <- t(regressLin(datos_hom, et)) #calculamos el vector de pesos por regresion
    et_reg <- apply(datos_hom, 1, h, w = w) #calculamos las etiquetas dadas por ese vector de pesos
    
    
    err_in = err_in + 0.001*length(which(et_reg != et)) #medimos el error dentro de la muestra
  }
  print("Apartado a")
  cat("El error promedio de entrenamiento es: ", err_in, "\n")
  #apartado b
  datos <- simula_unif(1000,2,c(-10,10)) #generamos datos
  datos_trans <- t(apply(datos,1, fi)) #generamos los datos (1,x,y,xy,x^2, y^2) a partir de los generados
  et <- introducirRuido(apply(datos, 1, f5)) #introducimos ruido a las etiquetas
  
  w_gorro <- t(regressLin(datos_trans, et)) #calculamos el vector de pesos por regresion
  et_reg <- apply(datos_trans, 1, h, w = w_gorro) #calculamos las etiquetas dadas por esos pesos
  print("Apartado b")
  cat("El w de pesos obtenido: ", w_gorro, "\n")
  cat("El error dentro de la muestra es: ", length(which(et != et_reg)), "\n")
  
  #hacemos un submuestreo de la region donde pintaremos la funcion
  x <- seq(-10,10,length = 1000)
  y <- seq(-10,10,length = 1000)
  #calculamos el valor de la funcion en esas muestras
  z <- outer(x,y,function(x,y) {w_gorro[1]+x*w_gorro[2]+y*w_gorro[3]+w_gorro[4]*x*y+w_gorro[5]*x*x+w_gorro[6]*y*y})
  
  plot(datos, col = et+2)
  #dibujamos la funcion con los datos indicados
  par(new = TRUE)
  contour(x,y,z,levels=0, drawlabels = FALSE)
  
  #apartado c
  err_out = 0
  for (i in seq(1,1000)) {
    datos <- simula_unif(1000,2,c(-10,10)) #generamos datos
    datos_trans <- t(apply(datos,1, fi)) #generamos los datos (1,x,y,xy,x^2, y^2) a partir de los generados
    et <- introducirRuido(apply(datos, 1, f5)) #introducimos ruido a las etiquetas
    
    w_gorro <- t(regressLin(datos_trans, et)) #calculamos el vector de pesos
    
    datos_out <- simula_unif(1000, 2, c(-10,10)) #generamos los datos fuera de la meustra para medir con ellos Eout
    datos_out_trans <- t(apply(datos_out, 1, fi)) #obtenemos los datos transformados 
    
    et_out <- apply(datos_out, 1, f5) #obtenemos etiquetas por la funcion
    et_out_w <- apply(datos_out_trans, 1, h, w = w_gorro) #obtenemos etiquetas por el vector de pesos obtenido
    
    err_out = err_out + 0.001*length(which(et_out != et_out_w)) #medimos el error fuera de la muestra
  }
  print("Apartado c")
  cat("E_out promedio: ", err_out, "\n")
}

print("Visualizacion y generazion de datos")
print("Ejercicio3")
ejercicio3()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 4")
ejercicio4()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 6")
ejercicio6()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 7")
ejercicio7()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 8")
ejercicio8()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio de PLA")
print("Ejercicio 2")
ejercicio3.2()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 3")
ejercicio3.3()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 4")
ejercicio3.4()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 5")
#ejercicio3.5()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 6")
ejercicio3.6()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))

print("Ejercicio de regresion lineal")
print("Ejercicios del 1 al 6")
ejercicio4.2()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 7")
ejercicio4.7()
print ("Pulsar ENTER para continuar.")
invisible(readLines("stdin", n=1))
print("Ejercicio 8")
ejercicio4.8()
print ("Pulsar ENTER para salir.")
invisible(readLines("stdin", n=1))