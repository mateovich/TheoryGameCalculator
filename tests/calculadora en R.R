# preámbulo
cc <- list(-1,-1)
cd <- list(0,-9)
dc <- list(-9, 0)
dd <- list(-6,-6)
payoff <- list(cc,cd,dc,dd)
prisioneDilema <- array(payoff, dim = c(2,2))

## Array de prueba 2


payoffArrayPrueba <- list(list(1,2,3),list(4,5,6), list(7,8,9),list(10,11,12),list(13,14,15),list(16,17,18),list(19,20,21),list(22,23,24))
ArrayPrueba <- array(payoffArrayPrueba, dim=c(2,2,2))

  
# prueba3
myarray <- array(1:8, dim = c(2,2,2))
mylist <- list(1,1,1)
myarray[mylist]


## Calculadora de combinaciones:
calucombi <- function(x) {
  jugadores <- length(x)
  totalResults <- 1
  totalCombis <- 0
  for (n in 1:jugadores) {
    totalResults <- x[n] * totalResults
  }
  for (n in x) {
    totalCombis <- totalCombis + ((factorial(n)) / (2 * (factorial(n - 2)))) * (totalResults / n)
  }
  print(totalCombis)
}



## Incrementador:
Incrementator <- function(conter, limit, increment) {
  conter <- as.integer(conter)
  limit <- as.integer(limit)
  cursor <- 1
  iterations <- 0
  Finish <- FALSE
  
  while (!Finish & (increment > iterations)) {
    if (identical(conter, limit)) {
      Finish <- TRUE
    } else {
      if (conter[[cursor]] < limit[[cursor]]) {
        conter[[cursor]] <- conter[[cursor]] + 1
        iterations <- iterations + 1
      } else {
        while (conter[[cursor]] == limit[[cursor]]) {
          cursor <- cursor + 1
        }
        conter[[cursor]] <- conter[[cursor]] + 1
        cursor <- cursor - 1
        conter[[cursor]] <- 1
        while (cursor > 1) {
          cursor <- cursor - 1
          conter[[cursor]] <- 1
        }
        iterations <- iterations + 1
      }
    }
  }
  
  return(list(conter, Finish))
}






## Spliter

spliter <- function(conterr, limit, selected, increment) {
  
  conterrOriginal <- conterr
  limitOriginal <- limit
  
  elementoEliminadoconter <- conterr[[selected]]
  conterSustracted <- conterr[-selected]
  
  elementoEliminadolimit <- limit[[selected]]
  limitSustracted <- limit[-selected]
  conterIncremented <- Incrementator(conterSustracted, limitSustracted, increment)[[1]]
  
  conterIncremented <- append(conterIncremented, after = selected - 1, values = elementoEliminadoconter)
  IsFinish <- Incrementator(conterSustracted, limitSustracted, increment)[[2]]
  
  return(list(conterIncremented, IsFinish))
}

# ---- Comparador ---
# Productor de combinaciones de un mismo jugador
Combi2_en_2 <- function(n) {
  if (n < 2) {
    return (NA)
  }
  numeros <- 1:n
  combinaciones <- combn(numeros, 2)
  return (t(combinaciones))
}

# sustituidor
sustituter <- function(matrix, value_to_sustitute, index) {
  matrix[[index]] <- value_to_sustitute
  return(matrix)
}

## Col Remover
removecol <- function(array, col_to_del,in_dim){
  qeo <- list(quote(expr=))
  qeo <- rep(qeo, length(dim(array)))
  qeo[in_dim] <- -col_to_del
  args <- c(qeo, drop = FALSE)
  out <- do.call(`[`, c(list(array), args))
  return(out)
}


# buscador de dominación y reductor
BuscarDominacion <- function(juego, msg = "") {
  playersNumber <- length(dim(juego))
  Limits <- list() 
  conter <- list()
  for (x in 1:playersNumber) {  # creador de limit
    Limits <- append(Limits, dim(juego)[x])
  }
  for (x in 1:playersNumber) { # crear conter que parta de 0 (lista [1,1,...,1])
    conter <- append(conter, 1)
  }
  
  for (x in 1:playersNumber) { # x = jugador presente
    combis <- Combi2_en_2(Limits[[x]])
    if (is.na(combis)) {
      next
    }
    for (y in 1:dim(combis)[1]) {# y = combinación presente
      statue <- "void"
      a <- combis[[y,1]]
      b <- combis[[y,2]]
      IsFinish <- FALSE
      conterActual <- conter
      while(IsFinish == FALSE) { # combinación de combinación de estrategias del resto de jugadores
        aIndex <- sustituter(matrix(unlist(conterActual),ncol = playersNumber, byrow = TRUE), a, x)
        bIndex <- sustituter(matrix(unlist(conterActual),ncol = playersNumber, byrow = TRUE), b, x)
        
        if (juego[aIndex][[1]][[x]] > juego[bIndex][[1]][[x]]) { # comparador
          if (statue == "void") {
            statue <- TRUE
          }
          else if (statue == FALSE)
            break
        } else if ((juego[aIndex][[1]][[x]] == juego[bIndex][[1]][[x]])) {
          break
        } else if (juego[aIndex][[1]][[x]] < juego[bIndex][[1]][[x]]) {
          if (statue == "void") {
            statue <- FALSE
          }
          else if (statue == TRUE) {
            break
          }
        }
        if (spliter(conterActual,Limits, x, 1)[[2]] == TRUE) {
          IsFinish <- TRUE
        }
        
        conterActual <- spliter(conterActual,Limits, x, 1)[[1]]
      }
      if (IsFinish == TRUE & statue == TRUE ) {
        msgAdd <- paste("Se eliminó la jugada", combis[[y,2]], "del jugador ", x, "por ser estrictamente inferior a su jugada", combis[[y, 1]], "\n")
        msg <- paste(msg, msgAdd)
        return(BuscarDominacion(removecol(juego, combis[[y,2]], in_dim = x),msg))
      }
      if (IsFinish == TRUE & statue == FALSE) {
        msgAdd <- paste("Se eliminó la jugada", combis[[y,1]], "del jugador ", x, "por ser estrictamente inferior a su jugada", combis[[y, 2]], "\n")
        msg <- paste(msg, msgAdd)
        return(BuscarDominacion(removecol(juego, combis[[y,1]], in_dim = x), msg))
      }
    }
  }
  cat(msg)
  return(juego)
}

# Coordenates to index
CorToInd <- function(coor, limits) {
  index <- 1
  conter <- list()
  for (x in 1:length(limits)) { # crear conter que parta de 0 (lista [1,1,...,1])
    conter <- append(conter, 1)
  }
  while(!(identical(as.integer(conter), as.integer(coor)))) {
    conter <- Incrementator(conter, limits, 1)[[1]]
    index <- index + 1 
  }
  return(index)
}

IndToCor <- function(index, limits) {
  conter <- list()
  for (x in 1:length(limits)) { # crear conter que parta de 0 (lista [1,1,...,1])
    conter <- append(conter, 1)
  }
  return(Incrementator(conter, limits, index-1)[[1]])
}
# ------- 







### Calculador de equilibros de Nash en estrategias puras

BuscarEN <- function(juego, msg="") {
  playersNumber <- length(dim(juego))
  Limits <- list() 
  conter <- list()
  for (x in 1:playersNumber) {  # creador de limit
    Limits <- append(Limits, dim(juego)[x])
  }
  for (x in 1:playersNumber) { # crear conter que parta de 0 (lista [1,1,...,1])
    conter <- append(conter, 1)
  }
  payoffsCont = 1 ## crear el index de payoffs
  for (x in juego) {
    payoffsCont = payoffsCont * x
  }
  PayoffsPoints = rep(c(0), each=payoffsCont) # crea una lista de 0, donde se van a puntear cada payoff por índice, cada posición representa el índice y el valor su puntaje
  
  for (x in 1:playersNumber) { # x = jugador presente
    IsFinishA <- FALSE
    conterActual <- conter
    while(IsFinish == FALSE) { # combinación de estrategias del resto de jugadores actual
      bestRespons <- list() # bestRespons es una lista cuyo primer elemento es la lista de mejores respuestas y el segundo element el pago de esas mejores respuestas
      IsFinishB <- FALSE
      actualPlay <- 1
      while (actualPlay >= Limits[[x]]) {
        Index <- sustituter(conterActual, actualPlay, x)
        if (length(bestRespons) == 0) {
          bestRespons[1] <- list(CorToInd(Index, Limits))
          bestRespons[2] <- juego[Index][[1]][[x]]
          actualPlay <- actualPlay +1
          next }
        else if (bestRespons[[2]] == juego[Index][[1]][[x]]) {
          bestRespons[1] <- append(bestRespons[1], CorToInd(Index, Limits))
          actualPlay <- actualPlay +1
          next
        } else if (bestRespons[[2]] < juego[Index][[1]][[x]]){
          bestRespons[1] <- list(CorToInd(Index, Limits))
          bestRespons[2] <- juego[Index][[1]][[x]]
          actualPlay <- actualPlay +1
          next
        } else {
          actualPlay <- actualPlay +1
        }
          
      }
      for (z in bestRespons[1]) { #posible error
        PayoffsPoints[z] <- PayoffsPoints[[z]] +1
      }
      
      if (spliter(conterActual,Limits, x, 1)[[2]] == TRUE) {
        IsFinish <- TRUE
      }
      
      conterActual <- spliter(conterActual,Limits, x, 1)[[1]]
      }
    }
    
  }
  
}






