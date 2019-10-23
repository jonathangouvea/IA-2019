source("Estado.R")

## Classe e métodos para o problema do Aspirador de Pó
Aspirador <- function(desc = c(1, 0, 0, 0, 0), pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return((obj1$desc[2] == obj2$desc[2]) & (obj1$desc[3] == obj2$desc[3]) & (obj1$desc[4] == obj2$desc[4]) & (obj1$desc[5] == obj2$desc[5]))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("Aspirador em: ", obj$desc[1], "\n")
  cat("[ SE SD ]: [", obj$desc[2], " ", obj$desc[3], "]\n")
  cat("[ IE ID ]: [", obj$desc[4], " ", obj$desc[5], "]\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  if(is.null(atual$desc))
    return(Inf)
  
  k <- 0
  h <- 0
  desc = atual$desc
  if(length(atual$desc) == 1)
  {
    desc = atual$desc[[1]]
  }
  
  for (i in 2:5) {
    if(as.numeric(desc[i]) > 0) {
      k <- k + 1
    }
  }
  if (as.numeric(desc[as.integer(desc[1])]) > 0) {h <- (2*k)}
  else {h <- (2*k) + 1}
  
  return(h)
}

geraFilhos.Aspirador <- function(obj) {
  filhos <- list()
  filhosAspirador <- list()
  desc <- obj$desc
  
  custo <- c(0, 0, 0)
  
  pos <- as.numeric(desc[1])
  SE <- as.numeric(desc[2])
  SD <- as.numeric(desc[3])
  IE <- as.numeric(desc[4])
  ID <- as.numeric(desc[5])
  
  if(pos == 2) { #Superior Esquerdo
    filhosAspirador <- list(
      c(pos, 0, SD, IE, ID), #1 - Limpar
      c(3, SE, SD, IE, ID), #2 - Mover para direita
      c(4, SE, SD, IE, ID) #3 - Mover para baixo
    )
    custo <- c(2, 1, 3)
  }
  
  if(pos == 3) { #Superior Direito
    filhosAspirador <- list(
      c(pos, SE, 0, IE, ID), #1 - Limpar
      c(5, SE, SD, IE, ID), #3 - Mover para baixo
      c(2, SE, SD, IE, ID) #4 - Mover para esquerda 
    )
    custo <- c(2, 3, 1)
  }
  
  if(pos == 4) { #Inferior Esquerdo
    filhosAspirador <- list(
      c(pos, SE, SD, 0, ID), #1 - Limpar
      c(5, SE, SD, IE, ID), #2 - Mover para direita
      c(2, SE, SD, IE, ID) #5 - Mover para cima
    )
    custo <- c(2, 1, 3)
  }
  
  if(pos == 5) { #Inferior Direito
    filhosAspirador <- list(
      c(pos, SE, SD, IE, 0), #1 - Limpar
      c(4, SE, SD, IE, ID), #4 - Mover para esquerda
      c(3, SE, SD, IE, ID) #5 - Mover para cima
    )
    custo <- c(2, 1, 3)
  }
  
  for(i in 1:3) {
    filho <- Aspirador(desc = filhosAspirador[[i]], pai = obj)
    filho$g <- custo[i] + obj[["g"]]
    filho$h <- heuristica(filho)
    filho$f <- filho[["g"]] + filho[["h"]]
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}