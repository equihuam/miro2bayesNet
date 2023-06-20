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

neticaMiro <- red2DNE(frames_data = datos_miro$frames,
                      variables = datos_miro$nodes,
                      arcs = datos_miro$arcs,
                      network_name = "Red_Produccion_Cafe")

write(neticaMiro, "test1.dne")

netMiro_bn <- miro2bnlearn(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
graphviz.plot(netMiro_bn, layout = "dot", )

lapply(cond_indepOnvar(miro_dag$indepCond, "rendimiento"), cat)

