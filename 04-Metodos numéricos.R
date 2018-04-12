
euler <- function(
  fooName,
  valInic,
  tiempoParar,
  NoIter,
  guardarCada,
  ...) {
  arg <- list(...)
  fn <- get(fooName)
  
  #Encuentra los argumentos provistos
  argName <- match.arg(names(arg), #arg provistos
                       formalArgs(fn), #arg existentes
                       several.ok = TRUE)
  #Nombra la lista con los nombres de los argumentos provistos
  names(arg) <- argName
  
  deltaT <- tiempoParar/NoIter
  
  val <- numeric()
  val[1] <- valInic
  
  valTmp <- numeric()
  valTmp <- val[1]
  
  #Completa la lista de argumentos con N[t-1]
  arg[[(length(arg) + 1)]] <- valInic
  totalArg <- length(arg)
  #Escribe todos los nomres de los argumentos, para do.call
  names(arg) <-
    formalArgs(fn)#Encuetra los nombres de los argumentos
  
  tiempo <- numeric()
  
  counter <- 0
  tNow <- 0
  tiempo[1] <- 0
  
  while (tNow < tiempoParar) {
    valTmp <- valTmp + do.call(fn, args = arg) * deltaT
    tNow <- tNow + deltaT
    arg[[totalArg]] <- valTmp
    counter <- counter + 1
    
    if (counter == guardarCada) {
      val <- append(x = val, values = valTmp)
      tiempo <- append(x = tiempo, values = tNow)
      counter <- 0
    }
  }
  
  return(list(
    poblacion = val,
    tiempo = tiempo,
    tNow = tNow,
    arg = arg,
    Dt = deltaT
  ))
  
}



diffG1 <- function(rm,N)N*rm

fig1 <- function(){

N0 <- 10

Resultados1 <- euler(fooName = "diffG1",valInic = N0,tiempoParar = 10,NoIter = 100,guardarCada = 10,rm=.22)

plot(Resultados1$tiempo,Resultados1$poblacion,
     type = "p", xlab = "Tiempo", ylab = "Tamaño de población",
     las = 1, pch = 21, bg = 1)
lines(Resultados1$tiempo,Resultados1$poblacion)
}

fig2 <- function(){
  
  N0 <- 10
  rm <- 0.22
  
  
  curve(N0*exp(rm*x), from = 0,to = 5,
        lwd=3, ylab="Tamaño de población" )
  
  D1a2 <- euler(fooName = "diffG1",
                valInic = N0,
                tiempoParar = 5,
                NoIter = 10,
                guardarCada = 1,rm=.22)
  
  D1a100 <- euler(fooName = "diffG1",
                  valInic = N0,
                  tiempoParar = 5,
                  NoIter = 100,
                  guardarCada = 10,rm=.22)
  
  lines(D1a2$tiempo,D1a2$poblacion, lwd = 1, col="gray")
  points(D1a2$tiempo,D1a2$poblacion,pch=21,bg=1)
  
  lines(D1a100$tiempo,D1a100$poblacion, lwd = 2, lty = 2,
        col = "gray")
  points(D1a100$tiempo,D1a100$poblacion,pch=21,bg=1)
}

fig3 <- function(){
Nvect <- numeric()
lastTime <- numeric()
sizes <- character()
N0 <- 10
rm <- 0.22

for( i in 1:10){
  step <- (2^i)
  
  res <- euler(fooName = "diffG1",
               valInic = N0,
               tiempoParar = 5,
               NoIter = step,
               guardarCada = 1,rm=.22)
  
  Nvect <- append(Nvect,res$poblacion[length(res$poblacion)])
  
  lastTime <- append(lastTime, res$tiempo[length(res$tiempo)])
  
  sizes <- append(sizes,paste0("1/", step))
  
  
  
}

rm(N0,rm,res)

stopifnot(all(lastTime==5))


  
  plot((diff(Nvect)/Nvect[-1])*100,axes=F, 
       xlab="Delta T",
       ylab = "Porcentaje de error contra el anterior",
       pch = 21,bg = 1)
  
  axis(side=1, at = 1:length(diff(Nvect)),labels=sizes[-1])
  
  axis(side = 2, at = 1:length(diff(Nvect)),
       labels=paste0(1:length(diff(Nvect)),"%"),las=1)
  
}