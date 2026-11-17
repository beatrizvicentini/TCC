library(ggplot2)
library(dplyr)

plotar_boxplot_pesos_completo <- function(padrao_nome = "^Matriz_Adj_comp_", corte = 50) {
  
  # 1. RESET GRÁFICO: Fecha qualquer dispositivo travado e reseta margens
  while (!is.null(dev.list())) dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1) # Margens padrão do R
  
  # 2. Coleta de dados (mesma lógica anterior)
  nomes_objetos <- ls(pattern = padrao_nome, envir = .GlobalEnv)
  lista_pesos <- list()
  
  for (nome in nomes_objetos) {
    matriz <- as.matrix(get(nome, envir = .GlobalEnv))
    pesos <- matriz[upper.tri(matriz)]
    pesos_filtrados <- pesos[pesos > corte]
    
    if (length(pesos_filtrados) > 0) {
      lista_pesos[[nome]] <- data.frame(
        Rede = nome,
        Peso = pesos_filtrados
      )
    }
  }
  
  if (length(lista_pesos) == 0) stop("Sem dados acima do corte.")
  df_total <- bind_rows(lista_pesos)
  
  # 3. Criar o Gráfico com GGPLOT2 forçando elementos
  p <- ggplot(df_total, aes(x = Rede, y = Peso, fill = Rede)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red") +
    coord_flip() +
    # Adicionando títulos e labels explicitamente
    labs(
      title = "Distribuição de Pesos das Conexões",
      subtitle = paste("Apenas pesos superiores a", corte),
      x = "Matrizes de Adjacência",
      y = "Número de Viagens (Peso)",
      fill = "Legenda de Redes"
    ) +
    # Forçar a exibição da legenda e um tema claro
    theme_bw() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 10)
    )
  
  # 4. Exibir
  print(p)
}

# Execute assim:
plotar_boxplot_pesos_completo(corte = 0.1)
