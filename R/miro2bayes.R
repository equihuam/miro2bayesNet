#' List all Miro board
#'
#' This function recovers a list of all the boards available to the user
#' identified by provided credentials.
#' #'
#' @param servMiro Name of the credential service as defined in keyring setup
#' @param user User name as defined n keyring setup
#' @export
miroBoards <- function(servMiro = "miro", user)
{
  credentials <- keyring::key_get(service = "miro", username = user)
  credentials <-  paste("Bearer", credentials)
  url <- "https://api.miro.com/v2/"
  object <- "boards"
  send_url <- paste0(url, object)


  response <- httr::VERB("GET", send_url,
                         httr::add_headers('authorization' = credentials),
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  tableros <- tibble::as_tibble(jsonlite::fromJSON(httr::content(response, "text",
                                                                 encoding = "utf-8"),
                                                   flatten = TRUE))
  return(tableros$data)
}


#' Get Miro data into R
#'
#' This function accesses data from a specified Miro board describing
#' the fundamental elements of a Bayesian network: nodes and arcs.
#' Sticky notes are assumed to represent nodes, and connectors are the arcs.
#' Nodes are expected to have one and only one label attached. This label
#' is used to indicate the variable name that corresponds to the node in
#' the database.
#' It uses the "keyring library" to deal with credentials. It is expected
#' that credentials are prepared in advance with keyring::key_set(
#' service = servMiro, username = user). You should provide the token
#' produced by Miro when prompted for it in a popup window.
#'
#' @param servMiro Name of the credential service as defined in keyring setup
#' @param user User name as defined n keyring setup
#' @param board_id id code of the Miro board to access
#' @return a list of nodes, arcs and frames attributes
#' @export
datosMiro <- function (servMiro = "miro", user, board_id)
{
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }

  # Condiciones básicas de acceso a Miro
  credentials <- keyring::key_get(service = servMiro, username = user)
  credentials <-  paste("Bearer", credentials)
  url_miro <- "https://api.miro.com/v2/"
  object <- "boards/"

  # Marcos presentes en el tablero
  send_url <- paste0(url_miro, object, board_id, "/items")

  queryString <- list(limit = "20",
                      type = "frame")

  response <- httr::VERB("GET", send_url,
                         httr::add_headers('authorization' = credentials),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  frames_data <- jsonlite::fromJSON(httr::content(response, "text",
                                                  encoding = "utf-8"),
                                    flatten = TRUE)
  frames_data <- frames_data$data

  # Descripción y atributos de las Variables: "Sticky notes"
  send_url <- paste0(url_miro, object, board_id, "/items")

  queryString <- list( limit = "40",
                       type = "sticky_note")

  response <- httr::VERB("GET", send_url,
                         httr::add_headers('authorization' = credentials),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  vars_data <- jsonlite::fromJSON(httr::content(response, "text",
                                    encoding = "utf-8"),
                            flatten = TRUE)
  vars_data <- vars_data$data

  variables <-  tibble::tibble(id = vars_data$id,
                               text = vars_data$data.content,
                               color = vars_data$style.fillColor,
                               frame_id = vars_data$parent.id,
                               x = as.integer(vars_data$position.x),
                               y = as.integer(vars_data$position.y))
  variables$text <- unlist(lapply(variables$text, cleanFun))

  # Nombres de los nodes

  # Obtiene la etiqet asociada a cada sticky note
  for (i in (1:length(variables$id)))
  {
    id <- variables$id[i]
    send_url <- paste0(url_miro, object, board_id, "/items/", id, "/tags")

    response <- httr::VERB("GET", send_url,
                           httr::add_headers('authorization' = credentials),
                           httr::content_type("application/octet-stream"),
                           httr::accept("application/json"))

    labels_data <- jsonlite::fromJSON(httr::content(response, "text",
                                                        encoding = "utf-8"),
                                flatten = TRUE)
    if (i == 1)
    {
      if (is.null(labels_data$tags$title))
      {
        tags <-  "-"
      } else
      {  tags <-  labels_data$tags$title}
    } else
    {
      if (is.null(labels_data$tags$title))
      {
        tags <-  c(tags, "-")
      } else
      {tags <-  c(tags, labels_data$tags$title) }
    }
  }

  variables <- variables %>% tibble::add_column(var = tags)

  # Datos de los arcos
  send_url <- paste0(url_miro, object, board_id, "/connectors")

  queryString <- list(limit = "50")

  response <- httr::VERB("GET", send_url,
                         httr::add_headers('authorization' = credentials),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  arcs_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "utf-8"),
                          flatten = TRUE)
  num_arcos <-  arcs_data$total

  arcs_data <-  tibble::as_tibble(arcs_data$data) %>% dplyr::select(endItem.id, startItem.id)

  # Los arcos tienen el id de los nodes que tocan, ahora agrego el dato "var" del paso anterior.
  arcs_data_full <-   arcs_data %>%
                             dplyr::filter(complete.cases(.)) %>%
                             dplyr::left_join(., variables, by = dplyr::join_by(endItem.id == id)) %>%
                             dplyr::left_join(., variables, by = dplyr::join_by(startItem.id == id)) %>%
                             dplyr::select(startItem.id, var.y, endItem.id, var.x) %>%
                             dplyr::rename(end_n = var.x, start_n = var.y)

  miro_datos <-  list(nodes = variables,
                      arcs = arcs_data_full,
                      frames = frames_data)
  return(miro_datos)
}


#' Assemble the DAG as recovered from Miro sticky notes and arcs
#'
#' This function builds a formal DAG sing DOT language and dagitty
#' Using dagitty it produces a list of implied conditional independence conditions.
#' Using ggdag pprodces a graphical representation of the DAG.
#'
#' @param nodes Node data as recovered from Miro board
#' @param arcos Arc data as recovered from Miro board
#' @return a list holding a ggdag plot, text list of
#' implied conditional independenceof nodes, arcs and frames attributes,
#' and the DAG itself as interpreted by dagitty.
#' @export
prepara_DAG <- function(nodes, arcs)
{
  # Componentes del DAG
  valid_arcs <- arcs[(arcs$start_n != "-") &
                           (arcs$end_n != "-") &
                           (!is.na(arcs$start_n)) &
                           (!is.na(arcs$end_n)),]
  dag_head <- 'dag {bb="0,0,1,1"\n'
  dag_arcs <- paste0(valid_arcs$start_n, " -> ",
                      valid_arcs$end_n,
                      "\n", collapse = "")


  # Armado de DAG: Considero sólo las variables conectadas
  dag_str_effective <- paste0(dag_head, dag_arcs, "}")
  dag_effective <- dagitty::dagitty(dag_str_effective)
  coord_dag_effective<- dagitty::coordinates(dagitty::graphLayout(dag_effective))
  dagitty::coordinates(dag_effective) <- coord_dag_effective

  # Grafico el DAG con ggdag, más estético
  dag_effective_gg <- ggdag::tidy_dagitty(dag_effective)

  dag_graph <- dag_effective_gg %>%
              ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                           color = name)) +
              ggdag::geom_dag_edges_arc(curvature = c(0, .5), edge_color = "gray50") +
              ggdag::geom_dag_text_repel(ggplot2::aes(label=name, color = name),
                                         size = 3, force = 3, max.overlaps = 25) +
              ggdag::geom_dag_point(ggplot2::aes(color = name), size = 2) +
              ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(),
                             axis.ticks.x = ggplot2::element_blank(),
                             axis.title.y = ggplot2::element_blank(),
                             axis.text.y = ggplot2::element_blank(),
                             axis.ticks.y = ggplot2::element_blank(),
                             legend.position = "")

  # DAG: Independencia condicional implicada
  ind_cond <- dagitty::impliedConditionalIndependencies(dag_str_effective)

  # Independencia condicional implicada en formato gráfico (Latex)
  for (i in ind_cond)
  {
    if (identical(i, ind_cond[[1]]))
    {
      if (length(i$Z) > 0 )
      {
        ind_cond_t <- tibble::tibble(line = paste0("$\\newcommand{\\indep}{\\perp \\\\!\\\\!\\\\! \\perp}$",
                                    "\n$",
                                    gsub("_", "~", i$X),
                                    "~ \\indep ~",
                                    gsub("_", "~", i$Y),
                                    "~~ |~",
                                    gsub("_", "~", paste0(i$Z, collapse = ",~")),
                                    "$\n ", collapse = " "))
      } else
      {
        ind_cond_t <- tibble::tibble(line = paste0("$\\newcommand{\\indep}{\\perp \\\\!\\\\!\\\\! \\perp}$",
                                     "\n$",
                                     gsub("_", "~", i$X),
                                     "~ \\indep ~",
                                     gsub("_", "~", i$Y),
                                     "$\n ", collapse = " "))
      }

    } else
    {
      if (length(i$Z) > 0 )
      {
        ind_cond_t <- ind_cond_t %>%
                      tibble::add_row(tibble::tibble(line = paste0("$",
                                             gsub("_", "~", i$X),
                                             "~ \\indep ~",
                                             gsub("_", "~", i$Y),
                                             "~~| ~",
                                             gsub("_", "~",
                                                  paste0(i$Z, collapse = ",~")),
                                             "$\n ", collapse = " ")))
                  } else
      {
        ind_cond_t <- ind_cond_t %>%
                      tibble::add_row(tibble::tibble(line = paste0("$",
                                             gsub("_", "~", i$X),
                                             "~ \\indep ~",
                                             gsub("_", "~", i$Y),
                                             "$\n ", collapse = " ")))
      }
    }
  }
  # Guarda documento **DNE** en disco
  DAG_datos <-  list(gg_dag = dag_graph,
                     indepCond = ind_cond_t,
                     dag = dag_effective)
  return(DAG_datos)
}


#' This funnction builds a representation of the Bayesian
#' Network recovered from Miro in a DNE file format that
#' Netica and other Bayesian Network application can reed.
#'
#' @param frames_data Frame data as recovered from Miro board
#' @param variables Node data as recovered from Miro board
#' @param arcos Arc data as recovered from Miro board
#' @return a string that contains the whole DNE document
#'
#' @export
red2DNE <- function(frames_data, variables, arcs, network_name)
{
  ## Transfiere datos a Netica
  valid_arcs <- arcs[(arcs$start_n != "-") &
                           (arcs$end_n != "-") &
                           (!is.na(arcs$start_n)) &
                           (!is.na(arcs$end_n)),]

  nodes_linked <- unique(c(valid_arcs$start_n, valid_arcs$end_n))
  nodes <- sapply(nodes_linked, function(key) list(parents = character()), simplify=F)

  # Agrega color del group y coordenadas a los nodes y prepara la traslación al origen común.
  for(n in names(nodes))
  {
    frame_id <- variables$frame_id[variables$var == n]
    if (!is.na(frame_id))
    {
      group <- strsplit(frames_data$data.title[frames_data$id == frame_id], " ")[[1]][1]
      color <- frames_data$style.fillColor[frames_data$id == frame_id]
      off_x <- frames_data$position.x[frames_data$id == frame_id]
      off_y <- frames_data$position.y[frames_data$id == frame_id]
    } else
    {
      group <- NA
      color <- NA
      off_x <- -300
      off_y <- 400
    }

    nodes[[n]]["x"] <- variables$x[variables$var == n] + off_x
    nodes[[n]]["y"] <- variables$y[variables$var == n] + off_y
    nodes[[n]]["color"] <- color
    nodes[[n]]["group"] <- group
  }

  # Valores de escalamiento
  min_x <- min(frames_data$position.x, sapply(nodes, function(d) d$x))
  min_y <- min(frames_data$position.y, sapply(nodes, function(d) d$y))
  max_x <- max(frames_data$position.x, sapply(nodes, function(d) d$x))
  max_y <- max(frames_data$position.y, sapply(nodes, function(d) d$y))
  esc_x  <- (max_x - min_x) / 1500
  esc_y  <- (max_y - min_y) / 800

  # Traslada y escala la posición de los nodes
  for(n in names(nodes))
  {
    nodes[[n]]["x"] <- as.integer((nodes[[n]][["x"]]  - min_x) / esc_x)
    nodes[[n]]["y"] <- as.integer((nodes[[n]][["y"]]  - min_y) / esc_y)
  }

  # Construye la lista de "parents" de cada nodo
  for (a_i in 1:length(valid_arcs$end_n))
  {
    node <- valid_arcs$end_n[[a_i]]
    parent <- valid_arcs$start_n[[a_i]]
    nodes[[node]]$parents = append(nodes[[node]]$parents, parent)
  }


  # Encabezado del archivo DNE para Netica
  epoch_time <- as.integer(Sys.time())  # tiempo en segundos desde 01-01-1970

  # Formación de groups
  groups <- frames_data %>%
    dplyr::select(group = data.title, color = style.fillColor) %>%
    dplyr::mutate(group = stringi::stri_trans_general(stringr::str_extract(group, ".*?(?= )"),
                                      "Latin-ASCII"),
                  color = stringr::str_replace(color, "#", "Color = 0x0")) %>%
    dplyr::mutate(group = stringr::str_sub(group))


  # Lista de nodes por group
  group_nodes = list()
  for (n in names(nodes))
  {
    g_i = stringi::stri_trans_general(nodes[[n]]$group, "Latin-ASCII")
    if (!is.na(g_i))
    {
      if (n == names(nodes)[1])
      {
        group_nodes[[g_i]] = n
      } else
      {
        group_nodes[[g_i]] = c(group_nodes[[g_i]], n)
      }
    }
  }

  # Construye las líneas NodeSet" que usa Netica para colorear nodes por groups
  for (g in groups$group)
  {
    if (!is.na(g))
    {
      color = groups$color[groups$group == g]
      g_temp <- paste0("    NodeSet ", g, " {", color, ";};\n", collapse = "")
      if (g == groups$group[1])
      {
        groups_dne <- g_temp
      } else
      {
        groups_dne <- c(groups_dne, g_temp)
      }
    }
  }

  # Arma la sección general del doc DNE (inclye NodeSets)
  doc_dne <- paste0("// ~->[DNET-1]->~\n",
                    "\n",
                    "// File created by EquihuaM at InstEco_MX ",
                    "using Netica 6.05 on Jun 03, 2023 at 03:09:37 UTC.\n",
                    "\n",
                    "bnet ", network_name, " {\n",
                    "autoupdate = TRUE;\n",
                    "whenchanged = ", epoch_time, ";\n\n",
                    "visual V1 {\n",
                    "    defdispform = BELIEFBARS;\n",
                    "    nodelabeling = TITLE;\n",
                    "    NodeMaxNumEntries = 50;\n",
                    '    nodefont = font {shape= "Arial"; size= 9;};\n',
                    '    linkfont = font {shape= "Arial"; size= 9;};\n',
                    "    ShowLinkStrengths = 1;\n",
                    "    windowposn = (26, 26, 1002, 383);\n",
                    "    resolution = 72;\n",
                    "    drawingbounds = (1500, 800);\n",
                    "    showpagebreaks = FALSE;\n",
                    "    usegrid = TRUE;\n",
                    "    gridspace = (6, 6);\n",
                    "    NodeSet Node {BuiltIn = 1; Color = 0x00e1e1e1;};\n",
                    "    NodeSet Nature {BuiltIn = 1; Color = 0x00f8eed2;};\n",
                    "    NodeSet Deterministic {BuiltIn = 1; Color = 0x00d3caa6;};\n",
                    "    NodeSet Finding {BuiltIn = 1; Color = 0x00c8c8c8;};\n",
                    "    NodeSet Constant {BuiltIn = 1; Color = 0x00ffffff;};\n",
                    "    NodeSet ConstantValue {BuiltIn = 1; Color = 0x00ffffb4;};\n",
                    "    NodeSet Utility {BuiltIn = 1; Color = 0x00ffbdbd;};\n",
                    "    NodeSet Decision {BuiltIn = 1; Color = 0x00dee8ff;};\n",
                    "    NodeSet Documentation {BuiltIn = 1; Color = 0x00f0fafa;};\n",
                    "    NodeSet Title {BuiltIn = 1; Color = 0x00ffffff;};\n",
                    paste0(groups_dne, collapse = ""),
                    "    PrinterSetting A {\n",
                    "        margins = (1270, 1270, 1270, 1270);\n",
                    "        };\n    };\n\n", collapse = "")

  # Prepara la sección que define a los nodes individualmente
  i <- 0
  dne_nodes <- character()
  for (nodo in names(nodes))
  {
    i <- i + 1
    dne_nodes <- c(dne_nodes, paste0("node ", nodo, " {\n",
                                     "    discrete = TRUE;\n",
                                     "    states = (A, B, C);\n",
                                     "    kind = NATURE;\n",
                                     "    chance = CHANCE;\n",
                                     "    parents = (",
                                     paste0(nodes[[nodo]]$parents, collapse = "," ),");\n",
                                     "    whenchanged = ", epoch_time, ";\n",
                                     "    visual V1 {\n",
                                     "        center = (", nodes[[nodo]]$x, ", ",
                                     nodes[[nodo]]$y, ");\n",
                                     "        height = ", i, ";\n",
                                     "        };\n",
                                     "    };\n", collapse = ""))
  }

  dne_gr_nodes <- tibble::tibble(group = names(group_nodes),
                         nodes = as.character(
                                    sapply(group_nodes,
                                           function(nodes) paste0(nodes, collapse = ","))))

  # Para finalizar el documento DNE hay qe agregar la lista de agrupamientos
  dne_nodes <- paste0(dne_nodes, collapse = "\n")
  doc_dne_completo <- paste0(doc_dne, dne_nodes,
                             paste0("NodeSet ", dne_gr_nodes$group,
                                    " {Nodes = (",
                                    dne_gr_nodes$nodes, ");};\n",
                                    collapse = "")
                             , "};", collapse = "")
  return(doc_dne_completo)
}


#' This function extracts the subset of the implied conditional
#' independences that containe an specified node name.
#'
#' @param indeps Frame data as recovered from the Miro board.
#' @param var target variable to filter the conditional independence list.
#' @return string containing the subset of statements of conditional independence.
#' @export
cond_indepOnvar <-  function (indeps, var)
{
  var <-  gsub("_", "~", var)
  sub_in_cond <- indeps %>%
                 dplyr::select(linea) %>%
                 dplyr::filter(grepl(paste0(var, "(?=.*?~~[|])"),
                                     indeps$linea, perl = TRUE))

  sub_in_cond <-  rbind("$\\newcommand{\\indep}{\\perp \\\\!\\\\!\\\\! \\perp}$\n",
                        sub_in_cond)
  return(sub_in_cond)
}


#' This function does a quick check on the numeric consistency
#' of the network attributes as interpreted from the data
#' found in the Miro board.
#'
#' @param variables Node data as recovered from Miro board
#' @param arcs Arcs links as recovered from Miro board.
#' @return tibble::tibble with numbers sumirizing the network structure.
#' @export
miro_validar <- function(variables, arcs)
{
  dag <- prepara_DAG(variables, arcs)
  acyclic <- "Graph is not acyclic"
  if (dagitty::isAcyclic(dag$dag))
  {
    acyclic <- "Graph is acyclic"
  }

  num_nodes <- length(variables$id)
  valid_arcs <- arcs[(arcs$start_n != "-") &
                           (arcs$end_n != "-") &
                           (!is.na(arcs$start_n)) &
                           (!is.na(arcs$end_n)),]

  nodes_linked <- unique(c(valid_arcs$start_n, valid_arcs$end_n))

  num_nodes_linked <- length(nodes_linked)
  nodes_without_var <- length(variables$var[variables$var == "-"])
  repeated_node_names <- num_nodes - nodes_without_var -
                           length(unique(variables$var[variables$var != "-"]))
  num_arcs <- length(arcs$endItem.id)
  num_valid_arcs <- length(valid_arcs$endItem.id)
  unlinked_arcs <- length(arcs$endItem.id) -
                          length(arcs$endItem.id)
  duplicated_arcs <- length(valid_arcs$start_n[duplicated(valid_arcs)])

  check <- tibble::tibble(acyclic, num_nodes, num_nodes_linked, nodes_without_var,
                               repeated_node_names, num_arcs, num_valid_arcs,
                               unlinked_arcs, duplicated_arcs)

  cat("Is it a TRUE DAG:    ", check$acyclic, "\n",
      "Number of nodes:     ", check$num_nodes, "\n",
      "Numb.linked nodes:   ", check$num_nodes_linked, "\n",
      "Nodes without var:   ", check$nodes_without_var, "\n",
      "Duplicated nodes:    ", check$repeated_node_names, "\n",
      "Number of arcs:      ", check$num_arcs, "\n",
      "Well connected arcs: ", check$num_valid_arcs, "\n",
      "Numb. Loose arcs:    ", check$unlinked_arcs, "\n",
      "Duplicated arcs:     ", check$duplicated_arcs, "\n",
      sep = "")
}


#' This function feeds the data from Miro into a bnlearn network.
#'
#' @param variables Data on Nodes as recovered from Miro board
#' @param arcs Data on arcs links as recovered from Miro board.
#' @return tibble::tibble with numbers sumirizing the network structure.
#' @export
miro2bnlearn <- function(nodes, arcs)
{
  nodes_active <- arcs[(arcs$start_n != "-") &
                                (arcs$end_n != "-") &
                                (!is.na(arcs$start_n)) &
                                (!is.na(arcs$end_n)), ]

  nodeset <- unique(c(nodes_active$start_n, nodes_active$end_n))

  net_base = bnlearn::empty.graph(nodeset)

  bnlearn::arcs(net_base, check.cycles = FALSE) <- as.matrix(nodes_active[, c("start_n", "end_n")])

  return(net_base)
}




