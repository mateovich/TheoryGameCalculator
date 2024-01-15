# Coordenates to index and index to coordenates
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
