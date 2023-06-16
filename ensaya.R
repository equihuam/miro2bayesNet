dbx_path <-  "C:/Users/equih/Documents/1 Nubes/Dropbox/Robert/Redes/DAG/"
library(devtools)
install_github("equihuam/miro2bayesNet")

devtools::document()

library(miro2bayes)
tableros <- miroBoards(servMiro = "miro", user = "miguel-token")
tableros[, c("name", "id")]
datos_miro <- datosMiro(servMiro = "miro", user = "miguel-token",
                        board_id = "uXjVMGRTvaE=")
t(miro_validar(variables = datos_miro$nodes, arcs = datos_miro$arcs))

miro_dag <- prepara_DAG(nodes = datos_miro$nodes, arcs = datos_miro$arcs)
miro_dag$gg_dag

cat(red2DNE(frames_data = datos_miro$frames,
        variables = datos_miro$nodes,
        arcs = datos_miro$arcs,
        network_name = "Red_Produccion_Cafe"))


library(bnlearn)

nodes <- datos_miro$arcs[(datos_miro$arcs$start_n != "-") &
                           (datos_miro$arcs$end_n != "-") &
                           (!is.na(datos_miro$arcs$start_n)) &
                           (!is.na(datos_miro$arcs$end_n)),]


nodeset <- unique(c(nodes$start_n, nodes$end_n))

e = empty.graph(nodeset)

arcs(e) <- as.matrix(nodes[, c("start_n", "end_n")])

