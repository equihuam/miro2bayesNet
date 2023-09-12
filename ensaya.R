#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install()
#BiocManager::install(c("graph", "Rgraphviz", "RBGL"), force = TRUE)
#install.packages("gRain")

library(devtools)
devtools::document()
install_github("equihuam/miro2bayesNet")
#
#dbx_path <-  "C:/Users/equih/Documents/1 Nubes/Dropbox/Robert/Redes/DAG/"
#

library(bnlearn)
library(bnviewer)
library(tidyverse)
library(miro2bayes)

tableros <- miroBoards(servMiro = "miro", user = "miguel-token")
tableros[, c("name", "id")]

tablero_tr <- tableros %>%
              filter(str_detect(name, "Copia.*sólo t0")) %>%
              select(id, name)

datos_miro <- getMiro(servMiro = "miro", user = "miguel-token",
                        board = tablero_tr)

miroValidation(datos_miro)

cond_indepOnvar(datos_miro, "rendimiento")

neticaMiro <- miro2DNE(datos_miro)

write(neticaMiro, "Café-sólo-t0.dne")

netMiro_bn <- miro2bnlearn(datos_miro)
netMiro_bn

variables <- tibble(var = datos_miro$nodes$var)


# Distingue por Tipo de Capital asignado a la variable
# Colores de interés:
#    blue: producido
#    dark_green: natural
#    gray: contexto
#    light_pink: contexto
#    light_yellow: social
#    orange: decidir
#    red: estatus
#    violet: humano

capitales <- tibble(color = c("blue", "dark_green", "gray", "light_pink", "light_yellow",
                              "orange", "red", "violet"),
                    capital = c("producido", "natural", "contexto", "contexto", "social",
                                "decidir", "estatus", "humano"))


gCap1 <- datos_miro$nodes %>%
  filter(color  == capitales$color[1]) %>%
  select(var) %>%
  inner_join(variables)

gCap2 <- datos_miro$nodes %>%
  filter(color  == capitales$color[2]) %>%
  select(var) %>%
  inner_join(variables)

gCap3 <- datos_miro$nodes %>%
  filter(color  == capitales$color[3]) %>%
  select(var) %>%
  inner_join(variables)

gCap4 <- datos_miro$nodes %>%
  filter(color  == capitales$color[4]) %>%
  select(var) %>%
  inner_join(variables)

gCap5 <- datos_miro$nodes %>%
  filter(color  == capitales$color[5]) %>%
  select(var) %>%
  inner_join(variables)

gCap6 <- datos_miro$nodes %>%
  filter(color  == capitales$color[6]) %>%
  select(var) %>%
  inner_join(variables)

gCap7 <- datos_miro$nodes %>%
  filter(color  == capitales$color[7]) %>%
  select(var) %>%
  inner_join(variables)

gCap8 <- datos_miro$nodes %>%
  filter(color  == capitales$color[8]) %>%
  select(var) %>%
  inner_join(variables)


# Distingue por tipo de variable
grp <- datos_miro$frames$id

gAmb1 <- datos_miro$nodes %>%
      filter(frame_id  == grp[1]) %>%
      select(var) %>%
      inner_join(variables)

gAmb2 <- datos_miro$nodes %>%
  filter(frame_id  == grp[2]) %>%
  select(var) %>%
  inner_join(variables)

gAmb3 <- datos_miro$nodes %>%
  filter(frame_id  == grp[3]) %>%
  select(var) %>%
  inner_join(variables)

gAmb4 <- datos_miro$nodes %>%
  filter(frame_id  == grp[4]) %>%
  select(var) %>%
  inner_join(variables)

graphviz.plot(netMiro_bn, layout = "dot")

viewer(netMiro_bn,
       bayesianNetwork.title = "Café sustentable",
       bayesianNetwork.subtitle = paste("Tablero Miro:", tablero_tr$name),
       edges.shadow = TRUE,
       node.colors = list(background = "white",
                          border = "black",
                          highlight = list(background = "#e91eba",
                                           border = "black")),

       clusters.legend.title = list(text = "<b>Leyenda</b> <br> Categorías de las variables",
                                    style = "font-size:14px;
                                             font-family:Arial;
                                             color:black;
                                             text-align:center;"),
       clusters.legend.options = list(
         list(label = "Incidencia",
              shape = "icon",
              icon = list(code = "f140",
                          size = 30,
                          color = "#e91e63")),
         list(label = "Contexto Físico",
              shape = "icon",
              icon = list(code = "f1ce",
                          size = 30,
                          color = "#03a9f4")),
         list(label = "Ecosistemas",
              shape = "icon",
              icon = list(code = "f192",
                          size = 30,
                          color = "#4caf50")),
         list(label = "Beneficios Sociales",
              shape = "icon",
              icon = list(code = "f10c",
                          size = 30,
                          color = "#ffc107"))),
       clusters = list(
         list(label = "Capital 1",
              shape = "icon",
              icon = list(code = "f140", color = "#e91e63"),
              nodes = as.list(gAmb1$var)),
         list(label = "Capital 2",
              shape = "icon",
              icon = list(code = "f1ce", color = "#03a9f4"),
              nodes = as.list(gAmb2$var)),
         list(label = "Capital 3",
              shape = "icon",
              icon = list(code = "f192", color = "#4caf50"),
              nodes = as.list(gAmb3$var)),
         list(label = "Capital 4",
              shape = "icon",
              icon = list(code = "f10c", color = "#ffc107"),
              nodes = as.list(gAmb4$var))),
       bayesianNetwork.enabled.interactive.mode = FALSE,
       bayesianNetwork.height = "700px")



