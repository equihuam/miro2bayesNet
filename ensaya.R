if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("graph", "Rgraphviz", "RBGL"))
install.packages("gRain")

library(tidyverse)

dbx_path <-  "C:/Users/equih/Documents/1 Nubes/Dropbox/Robert/Redes/DAG/"
library(devtools)
install_github("equihuam/miro2bayesNet")

devtools::document()

library(miro2bayes)
tableros <- miroBoards(servMiro = "miro", user = "miguel-token")
tableros[, c("name", "id")]
datos_miro <- datosMiro(servMiro = "miro", user = "miguel-token",
                        board_id = tableros$id[grepl("sólo t0", tableros$name)])

miro_validar(variables = datos_miro$nodes, arcs = datos_miro$arcs)

miro_dag <- prepara_DAG(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
miro_dag$gg_dag

cond_indepOnvar(miro_dag$indepCond, "rendimiento")

neticaMiro <- red2DNE(frames_data = datos_miro$frames,
                      variables = datos_miro$nodes,
                      arcs = datos_miro$arcs,
                      network_name = "Red_Produccion_Cafe")

#write(neticaMiro, "test1.dne")

library(bnlearn)
library(bnviewer)
library(tidyverse)
netMiro_bn <- miro2bnlearn(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
netMiro_bn
acyclic(netMiro_bn, debug = TRUE)

graphviz.plot(netMiro_bn, layout = "dot",
              highlight = list(nodes = c(g1$var[c(-4, -5)], g2$var),
                               fill =  "blue",
                               col = "blue"))
viewer(netMiro_bn, bayesianNetwork.title = "Café sustentable",
       edges.shadow = TRUE,
       node.colors = "brown")


grp <- unique(datos_miro$nodes$frame_id)
g1 <- datos_miro$nodes %>% filter(frame_id  == grp[1])
g2 <- datos_miro$nodes %>% filter(frame_id  == grp[2])



