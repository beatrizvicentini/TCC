library(dynsbm)

# 1. Definição de caminhos e vetores
valores_comunidades <- c(25, 20, 15, 14, 13, 12, 11, 10, 5, 1)
caminho_base <- "C:/Users/catit/Downloads/rstudio-export (1)/"

# 2. Criar uma lista para armazenar os modelos completos e um vetor para os logliks
modelos_completos <- list()
loglik_vetor <- c()

for (i in valores_comunidades) {
  nome_arquivo <- paste0(caminho_base, "Obj_dynsbm_", i, "_comunidades.RData")
  
  if (file.exists(nome_arquivo)) {
    # Carrega o ficheiro (cria o objeto list.dynsbm_bin)
    load(nome_arquivo)
    
    # Captura o conteúdo e armazena na lista usando o 'i' como identificador
    # Usamos get() para extrair o conteúdo do nome fixo 'list.dynsbm_bin'
    nome_objeto_na_lista <- paste0("modelo_", i)
    modelos_completos[[nome_objeto_na_lista]] <- get("list.dynsbm_bin")
    
    # Extrai o loglikelihood para um vetor de resumo (facilita a consulta rápida)
    valor_log <- modelos_completos[[nome_objeto_na_lista]][[1]][["loglikelihood"]]
    loglik_vetor[as.character(i)] <- valor_log
    
    message("Modelo ", i, " carregado e armazenado com sucesso.")
  } else {
    warning("Ficheiro não encontrado: ", nome_arquivo)
  }
}

# 3. Como aceder aos modelos agora que estão na memória:

# Para ver o resumo do modelo de 20 comunidades:
# print(modelos_completos$modelo_20)

# Para aceder especificamente ao loglikelihood do modelo de 10:
# modelos_completos$modelo_10[[1]]$loglikelihood

# 4. Criar um quadro de resumo (Dataframe)
df_resumo <- data.frame(
  Q = as.numeric(names(loglik_vetor)),
  loglikelihood = as.numeric(loglik_vetor)
)

print(df_resumo)

plot(df_resumo)


library(dplyr)

# 1. Garantir que o resumo esteja ordenado por Q crescente
df_ordenado <- df_resumo %>% arrange(Q)

# 2. Criar o dataframe de diferenças
df_deltas <- df_ordenado %>%
  mutate(
    # Calcula a diferença entre o loglik atual e o anterior
    diff_loglikelihood = loglikelihood - lag(loglikelihood),
    # Calcula o ganho percentual (opcional, mas ajuda na análise)
    ganho_relativo_pct = (diff_loglikelihood / abs(lag(loglikelihood))) * 100
  ) %>%
  # Remover a primeira linha (pois não tem anterior para comparar)
  filter(!is.na(diff_loglikelihood))

# 3. Visualizar o resultado
print("Diferença de Log-Likelihood entre saltos de Q:")
print(df_deltas)

write.csv(df_deltas, "delta_loglikelihood_modelos.csv", row.names = FALSE)


# Instale se não tiver: install.packages("xtable")
library(xtable)

# Criar a tabela LaTeX
# "digits = 4" controla as casas decimais
tabela_latex <- xtable(df_deltas, 
                       caption = "Diferença de Log-likelihood entre os modelos $Q$ e $Q_{anterior}$",
                       label = "tab:delta_loglik")

# Imprimir o código
print(tabela_latex, include.rownames = FALSE, hline.after = c(-1, 0, nrow(df_deltas)))




# connectivity.plot(modelos_completos$modelo_20[[1]], Y_bin)
# adjacency.plot(modelos_completos$modelo_20[[1]], Y_bin, present=NULL, col=heat.colors(9))

# 1. Definir a função exatamente como na imagem
calc_pen_imagem <- function(Q, N, T_dim) {
  termo1 <- 0.5 * Q * log((N * (N - 1) * T_dim) / 2)
  termo2 <- 0.5 * ((Q * (Q - 1)) / 2) * T_dim * log((N * (N - 1)) / 2)
  return(termo1 + termo2)
}

# 2. Capturar as dimensões (Certifique-se de que Y_bin existe)
N_estacoes <- dim(Y_bin)[1]
T_tempo <- dim(Y_bin)[3]

# 3. Criar o DF calculando tudo internamente
df_icl <- df_resumo %>%
  rowwise() %>% # Isso garante que o cálculo use o Q de cada linha
  mutate(
    penalizacao = calc_pen_imagem(Q, N_estacoes, T_tempo),
    ICL = loglikelihood - penalizacao
  ) %>%
  ungroup() %>%
  arrange(desc(ICL))

# 4. Verificação
print(df_icl)


library(dplyr)

# 1. Calcular as métricas de comparação
# O arrange(Q) é essencial para que o 'lag' compare o Q atual com o anterior corretamente
df_icl_completo <- df_icl %>%
  arrange(Q) %>% 
  mutate(
    # Diferença absoluta entre o ICL atual e o anterior
    ganho_abs_icl = ICL - lag(ICL),
    
    # Ganho relativo em porcentagem
    # Usamos abs() no denominador para garantir que o sinal reflita a direção da melhoria
    ganho_rel_pct = (ganho_abs_icl / abs(lag(ICL))) * 100
  )

# Visualizar no console para conferência
print(df_icl_completo)


library(xtable)

# 2. Configurar a tabela para o formato LaTeX
# Selecionamos apenas as colunas principais para a tabela não ficar larga demais
tabela_para_latex <- df_icl_completo %>%
  select(Q, loglikelihood, penalizacao, ICL, ganho_rel_pct)
# Gráfico de dispersão com linhas ligando os pontos
# 1. Definir os limites do eixo Y para que as duas linhas caibam no gráfico
y_min <- min(df_icl_completo$ICL)
y_max <- max(df_icl_completo$loglikelihood)

# 2. Plotar a primeira linha (Log-likelihood)
plot((df_icl_completo$Q +1), df_icl_completo$loglikelihood, 
     type = "b", pch = 19, col = "lightslateblue",
     ylim = c(y_min, y_max),
     xlab = "Número de Comunidades (Q)", 
     ylab = "Valor",
     main = "Log-likelihood vs ICL")

# 3. Adicionar a segunda linha (ICL)
lines((df_icl_completo$Q+1), df_icl_completo$ICL, 
      type = "b", pch = 18, col = "springgreen2")

# 4. Adicionar uma legenda para identificar as linhas
legend("bottomright", 
       legend = c("Log-likelihood", "ICL"), 
       col = c("lightslateblue", "springgreen2"), 
       lty = 1, pch = c(19, 18))

# Opcional: Adicionar uma grade para facilitar a leitura
grid()
# Criar o objeto xtable
xtab <- xtable(tabela_para_latex, 
               caption = "Critérios de Seleção de Modelo e Ganhos de Verossimilhança por Comunidade ($Q$)",
               label = "tab:selecao_modelo_icl",
               align = "cccccc") # Alinhamento centralizado para todas as colunas

# Imprimir o código final (copie o resultado do console para o Overleaf)
print(xtab, 
      include.rownames = FALSE, 
      booktabs = TRUE, 
      format.args = list(big.mark = ".", decimal.mark = ","), # Padrão brasileiro
      NA.string = "---") # Substitui o primeiro ganho (que é NA) por traços



# k <- 100
# lista <- c(10:15)
#arquivo_log <- "log_likelihood_comunidades.txt"

# if (!file.exists(arquivo_log)) {
#   cat("Comunidades(i)\tLogLikelihood\n", file = arquivo_log)
# }

# for (i in lista){
#   list.dynsbm_bin <- select.dynsbm(Y_bin, Qmin=i, Qmax=i, edge.type="binary", nstart=k)
#   nome_arquivo <- paste0("Obj_dynsbm_", i, "_comunidades.RData")
#   save(list.dynsbm_bin, file = nome_arquivo)
#   valor_loglik <- list.dynsbm_bin$loglikelihood
#   # Escreve no TXT: o valor de i, um tab (\t), o valor do loglik e uma quebra de linha (\n)
#   # cat(i, "\t", valor_loglik, "\n", file = arquivo_log, append = TRUE)
#   print(paste("Finalizado i =", i))
# }


