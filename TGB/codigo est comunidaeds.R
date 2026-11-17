library(dynsbm)





# ---------------------------------------------------------------------------------
k <- 100 #quantidade de inicializações do algoritmo para evitar maximos locais
lista <- c(1, 20, 15, 10, 5)
arquivo_log <- "log_likelihood_comunidades.txt"

if (!file.exists(arquivo_log)) {
  cat("Comunidades(i)\tLogLikelihood\n", file = arquivo_log)
}

for (i in lista){
  list.dynsbm_bin <- select.dynsbm(Y_bin, Qmin=i, Qmax=i, edge.type="binary", nstart=k)
  nome_arquivo <- paste0("Obj_dynsbm_", i, "_comunidades.RData")
  save(list.dynsbm_bin, file = nome_arquivo)
  valor_loglik <- list.dynsbm_bin[[i]]$loglikelihood
  # Escreve no TXT: o valor de i, um tab (\t), o valor do loglik e uma quebra de linha (\n)
  cat(i, "\t", valor_loglik, "\n", file = arquivo_log, append = TRUE)
  print(paste("Finalizado i =", i))
}


ll <- c()
dif <- c()
#x_bin <- melhor # de grupos
for (i in 1:15){
  ll[i] <- list.dynsbm_bin[[i]]$loglikelihood
  if (i==1){
    dif[i] <- 0
  } else{
    dif[i] <- ll[i-1] - ll[i]
  }
  
}

plot(ll)
plot(dif[-1])
plot(exp(dif[-1]))
dynsbm_bin <- list.dynsbm_bin[[15]]

save(dynsbm_bin, file = "Estimacao_comunidades_dynsbm.RData")
#------------------------------------------------------------------------

list.dynsbm_comp <- select.dynsbm(Y_comp, Qmin=15, Qmax=20, edge.type="discrete", nstart=k)

#x_comp <- melhor # de grupos


dynsbm_comp <- list.dynsbm_comp[[x_comp]]