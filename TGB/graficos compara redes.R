# library(igraph)
# 
# # 1. Definir os objetos específicos
# objetos_selecionados <- c("Matriz_Adj_comp_1", "Matriz_Adj_comp_2", "Matriz_Adj_comp_3")
# objetos_selecionados <- c("Matriz_Adj_comp_1", "Matriz_Adj_comp_12", "Matriz_Adj_comp_22")
# 
# # 2. Configuração do arquivo de saída (única imagem para os 3 grafos)
# dir_plot <- "D:/User/Documentos/bia/2025/TGA/bikeshare/datasets/redes/imagens"
# dir.create(dir_plot, recursive = TRUE, showWarnings = FALSE)
# arquivo_final <- file.path(dir_plot, "comparativo_1_12_22_estagios.png")
# 
# # 3. Iniciar dispositivo gráfico (Largura aumentada para comportar 3 plots)
# png(arquivo_final, width = 4500, height = 1500, res = 300)
# 
# # Configura a grade: 1 linha, 3 colunas
# # Margens pequenas para cada gráfico individual
# par(mfrow = c(1, 3), mar = c(1, 0.2, 3, 0.2), bg = "white")
# 
# for (nome_matriz in objetos_selecionados) {
#   
#   # Preparar matriz e grafo
#   matriz <- as.matrix(get(nome_matriz, envir = .GlobalEnv))
#   g <- graph_from_adjacency_matrix(matriz, mode = "undirected", weighted = TRUE, diag = FALSE)
#   
#   # Filtro: Manter apenas arestas com peso >= 50 (conforme solicitado anteriormente)
#   g <- delete_edges(g, E(g)[weight < 50])
#   
#   # Lógica de Cores e Larguras
#   pesos <- E(g)$weight
#   cores_finais <- rep(NA, length(pesos))
#   larguras <- rep(1, length(pesos))
#   
#   # Estágio: 50 a 100 (Amarelo) - Mantendo sua lógica de largura = 1
#   cores_finais[pesos >= 50 & pesos <= 100] <- "#FFD700"
#   larguras[pesos >= 50 & pesos <= 100] <- 1
#   
#   # Estágio: 100 a 300 (Laranja)
#   cores_finais[pesos > 100 & pesos <= 300] <- "#FF8C00"
#   larguras[pesos > 100 & pesos <= 300] <- 1.5
#   
#   # Estágio: > 300 (Vermelho)
#   cores_finais[pesos > 300] <- "red"
#   larguras[pesos > 300] <- 2.5
#   
#   # Plotagem individual dentro da grade
#   plot(
#     g,
#     vertex.color = "black",
#     vertex.size = 1,
#     vertex.frame.color = NA,
#     vertex.label = NA,
#     edge.color = cores_finais,
#     edge.width = larguras,
#     layout = layout_final, # Usando seu layout global
#     rescale = TRUE,
#     margin = -0.05,
#     main = nome_matriz # Título acima de cada grafo para identificação
#   )
# }
# 
# dev.off()
# message("Gráfico comparativo salvo em: ", arquivo_final)

layout_coords <- infos[, c(3,4)]
layout_coords <- as.matrix(
  data.frame(
    lapply(layout_coords, function(x) as.numeric(as.character(x)))
  )
)


library(igraph)

# 1. Definir os objetos e seus respectivos títulos
objetos_selecionados <- c("Matriz_Adj_comp_1", "Matriz_Adj_comp_2", "Matriz_Adj_comp_3")
objetos_selecionados <- c("Matriz_Adj_comp_1", "Matriz_Adj_comp_12", "Matriz_Adj_comp_22")
titulos_customizados <- c("Janeiro 2024", "Fevereiro 2024", "Março 2024") # Exemplo de vetor de Strings
titulos_customizados <- c("Janeiro 2024", "Dezembro 2024", "Outubro 2025") # Exemplo de vetor de Strings


# 2. Configuração do arquivo de saída
dir_plot <- "D:/User/Documentos/bia/2025/TGA/bikeshare/datasets/redes/imagens"
dir.create(dir_plot, recursive = TRUE, showWarnings = FALSE)
arquivo_final <- file.path(dir_plot, "comparativo_1_12_22_estagios.png")

# 3. Iniciar dispositivo gráfico
png(arquivo_final, width = 4500, height = 1500, res = 300)

# Configura a grade
par(mfrow = c(1, 3), mar = c(1, 0.2, 5, 0.2), bg = "white")

# Usamos um loop por índice (1 a 3) para sincronizar matriz e título
for (i in 1:length(objetos_selecionados)) {
  
  nome_matriz <- objetos_selecionados[i]
  titulo_atual <- titulos_customizados[i]
  
  # Preparar matriz e grafo
  matriz <- as.matrix(get(nome_matriz, envir = .GlobalEnv))
  g <- graph_from_adjacency_matrix(matriz, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  # Filtro: Manter apenas arestas com peso >= 50
  g <- delete_edges(g, E(g)[weight < 50])
  
  # Lógica de Cores e Larguras
  pesos <- E(g)$weight
  cores_finais <- rep(NA, length(pesos))
  larguras <- rep(1, length(pesos))
  
  cores_finais[pesos >= 50 & pesos <= 100] <- "#FFD700"
  larguras[pesos >= 50 & pesos <= 100] <- 1
  
  cores_finais[pesos > 100 & pesos <= 300] <- "#FF8C00"
  larguras[pesos > 100 & pesos <= 300] <- 1.5
  
  cores_finais[pesos > 300] <- "red"
  larguras[pesos > 300] <- 2.5
  
  # Plotagem
  plot(
    g,
    vertex.color = "black",
    vertex.size = 1,
    vertex.frame.color = NA,
    vertex.label = NA,
    edge.color = cores_finais,
    edge.width = larguras,
    layout = layout_coords, 
    rescale = TRUE,
    margin = -0.05,
    main = titulo_atual,   # Aqui o título é selecionado do vetor de Strings
    cex.main = 2           # Aumenta um pouco o tamanho da fonte do título
  )
}

dev.off()
message("Gráfico comparativo salvo com títulos customizados!")
