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
