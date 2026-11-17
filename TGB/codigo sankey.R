library(networkD3)
library(dplyr)
library(stringr)
library(paletteer)
library(htmlwidgets)

# 1. Configuração de Cores e Datas
cores_hex <- as.character(paletteer_c("grDevices::Set 3", 30)[20:30])
minhas_cores <- paste0('d3.scaleOrdinal().domain(["', paste0(0:10, collapse='","'), '"]).range(["', paste0(cores_hex, collapse='","'), '"]);')

# Definindo os rótulos das datas para os índices 1, 12 e 21
labels_datas <- c("Jan/2024", "Out/2024", "Out/2025")

# 2. Processamento dos Dados (Índices 1, 12, 21)
indices_selecionados <- c(1, 10, 21)
dados_memb <- as.data.frame(list.dynsbm_bin[[1]]$membership[, indices_selecionados])
colnames(dados_memb) <- paste0("T", indices_selecionados)

# 3. Links e Nodes
links <- data.frame()
for (i in 1:(length(indices_selecionados) - 1)) {
  links <- rbind(links, dados_memb %>%
                   group_by(source = paste0("T", indices_selecionados[i], "_C", .data[[paste0("T", indices_selecionados[i])]]), 
                            target = paste0("T", indices_selecionados[i+1], "_C", .data[[paste0("T", indices_selecionados[i+1])]])) %>%
                   summarise(value = n(), .groups = 'drop'))
}

nodes <- data.frame(name = unique(c(links$source, links$target))) %>%
  mutate(
    tempo = as.numeric(str_extract(name, "(?<=T)\\d+")),
    classe = as.numeric(str_extract(name, "(?<=_C)\\d+")),
    group = as.character(classe),
    label = ifelse(tempo == indices_selecionados[1], as.character(classe), "")
  ) %>%
  arrange(match(tempo, indices_selecionados), classe)

links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# 4. Renderização com Margem Superior
p <- sankeyNetwork(Links = as.data.frame(links), Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "label", NodeGroup = "group",
                   fontSize = 25, nodeWidth = 40, sinksRight = FALSE,
                   iterations = 0, colourScale = minhas_cores,
                   margin = list(top = 50, right = 20, bottom = 10, left = 20)) 

# 5. Injeção das Datas com Alinhamento nos Nós
p <- onRender(p, sprintf('
  function(el, x) {
    var datas = %s;
    var svg = d3.select(el).select("svg");
    
    // 1. Estilo da página
    d3.select("body").style("margin", "0").style("overflow", "hidden");
    svg.style("width", "100vw").style("height", "95vh");

    // 2. Capturar as posições X únicas de todas as colunas de nós
    var xPosicoes = [];
    d3.select(el).selectAll(".node").each(function(d) {
      if (xPosicoes.indexOf(d.x) === -1) {
          xPosicoes.push(d.x);
      }
    });
    xPosicoes.sort(function(a, b) { return a - b; });

    // 3. Adicionar as datas exatamente sobre a posição X de cada coluna
    // d.x representa o lado esquerdo do nó, somamos nodeWidth/2 (20) para centralizar
    datas.forEach(function(d, i) {
      if (xPosicoes[i] !== undefined) {
        svg.append("text")
          .attr("x", xPosicoes[i] + 20) 
          .attr("y", 35)
          .attr("text-anchor", "middle")
          .style("font-family", "sans-serif")
          .style("font-weight", "bold")
          .style("font-size", "22px")
          .style("fill", "#333")
          .text(d);
      }
    });

    // 4. Centralizar números das classes dentro dos nós
    d3.select(el).selectAll(".node text")
      .attr("x", 20)
      .attr("text-anchor", "middle")
      .style("fill", "#000")
      .style("font-weight", "bold");
  }
', jsonlite::toJSON(labels_datas)))

p