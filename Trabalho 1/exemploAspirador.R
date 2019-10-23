debugSource("Aspirador.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- Aspirador(desc = c(2, 0, 1, 1, 1))

objetivo <- Aspirador()
objetivo$desc <- c(2, 0, 0, 0, 0)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))