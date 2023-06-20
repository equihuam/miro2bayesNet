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
miro_dag$indepCond$line[2]

neticaMiro <- red2DNE(frames_data = datos_miro$frames,
                      variables = datos_miro$nodes,
                      arcs = datos_miro$arcs,
                      network_name = "Red_Produccion_Cafe")

write(neticaMiro, "test2.dne")

netMiro_bn <- miro2bnlearn(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
graphviz.plot(netMiro_bn, layout = "dot", )

cond_indepOnvar(miro_dag$indepCond, "rendimiento")
miro_dag$indepCond[2]

nodos_color <- datos_miro$nodes %>%
               left_join(datos_miro$frames, suffix = c("a", "b"),
                         by = join_by(frame_id == id), )




indeps %>%
  dplyr::select(line) %>%
  dplyr::filter(grepl(paste0("hidro", "(?=.*?~~[|])"),
                      indeps$line, perl = TRUE))
