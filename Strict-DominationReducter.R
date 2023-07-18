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
# pruductor de combinaciones de los jugadores restantes
CombiX_en_X <- function(Limits, selected) {
  
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
BuscarDominacion <- function(juego) {
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
        return(BuscarDominacion(removecol(juego, combis[[y,2]], in_dim = x)))
      }
      if (IsFinish == TRUE & statue == FALSE) {
        return(BuscarDominacion(removecol(juego, combis[[y,1]], in_dim = x)))
      }
    }
  }
  return(juego)
}
