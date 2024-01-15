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