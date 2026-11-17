library(dynsbm)

k <- 100 #quantidade de inicializações do algoritmo para evitar maximos locais

list.dynsbm_bin <- select.dynsbm(Y_bin, Qmin=1, Qmax=20, edge.type="binary", nstart=k)

#x_bin <- melhor # de grupos


dynsbm_bin <- list.dynsbm_bin[[x_bin]]

#------------------------------------------------------------------------

list.dynsbm_comp <- select.dynsbm(Y_comp, Qmin=1, Qmax=20, edge.type="discrete", nstart=k)

#x_comp <- melhor # de grupos


dynsbm_comp <- list.dynsbm_comp[[x_comp]]