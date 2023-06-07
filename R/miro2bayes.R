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
#' @return a list of nodes, arcs and frames attributes
#' @export
datosMiro <- function (servMiro = "miro", user)
{
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }

  # Condiciones básicas de acceso a Miro
  credenciales <- keyring::key_get(service = "miro", username = user)
  credenciales <-  paste("Bearer", credenciales)
  url_miro <- "https://api.miro.com/v2/"

  # Tablero de Café: Modelo Causal (sólo t0)
  tablero_cafe = "uXjVMGRTvaE="
  objeto <- "boards/"

  # Marcos presentes en el tablero
  enviar_url <- paste0(url_miro, objeto, tablero_cafe, "/items")
  queryString <- list(limit = "20",
                      type = "frame")

  response <- httr::VERB("GET", enviar_url,
                         httr::add_headers('authorization' = credenciales),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  datos_marcos <- jsonlite::fromJSON(httr::content(response, "text",
                                             encoding = "utf-8"), flatten = TRUE)
  datos_marcos <- datos_marcos$data

  # Descripción y atributos de las Variables: "Sticky notes"
  enviar_url <- paste0(url_miro, objeto, tablero_cafe, "/items")

  queryString <- list( limit = "40",
                       type = "sticky_note")

  response <- httr::VERB("GET", enviar_url,
                         httr::add_headers('authorization' = credenciales),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  datos_pegotes <- jsonlite::fromJSON(httr::content(response, "text",
                                    encoding = "utf-8"),
                            flatten = TRUE)
  datos_pegotes <- datos_pegotes$data

  papelitos <-  tibble::tibble(id = datos_pegotes$id,
                               texto = datos_pegotes$data.content,
                               color = datos_pegotes$style.fillColor,
                               frame_id = datos_pegotes$parent.id,
                               x = as.integer(datos_pegotes$position.x),
                               y = as.integer(datos_pegotes$position.y))
  papelitos$texto <- unlist(lapply(papelitos$texto, cleanFun))

  # Nombres de los Nodos

  # Obtiene la etiqet asociada a cada sticky note
  for (i in (1:length(papelitos$id)))
  {
    id <- papelitos$id[i]
    enviar_url <- paste0(url_miro, objeto, tablero_cafe, "/items/", id, "/tags")

    response <- httr::VERB("GET", enviar_url,
                           httr::add_headers('authorization' = credenciales),
                           httr::content_type("application/octet-stream"),
                           httr::accept("application/json"))

    datos_etiquetas <- jsonlite::fromJSON(httr::content(response, "text", encoding = "utf-8"),
                                flatten = TRUE)
    if (i == 1)
    {
      if (is.null(datos_etiquetas$tags$title))
      {
        tags <-  "-"
      } else
      {  tags <-  datos_etiquetas$tags$title}
    } else
    {
      if (is.null(datos_etiquetas$tags$title))
      {
        tags <-  c(tags, "-")
      } else
      {tags <-  c(tags, datos_etiquetas$tags$title) }
    }
  }

  papelitos <- papelitos %>% add_column(var = tags)

  # Datos de los arcos
  enviar_url <- paste0(url_miro, objeto, tablero_cafe, "/connectors")

  queryString <- list(limit = "50")

  response <- httr::VERB("GET", enviar_url,
                         httr::add_headers('authorization' = credenciales),
                         query = queryString,
                         httr::content_type("application/octet-stream"),
                         httr::accept("application/json"))

  datos_arcos <- jsonlite::fromJSON(httr::content(response, "text", encoding = "utf-8"),
                          flatten = TRUE)
  num_arcos <-  datos_arcos$total

  datos_arcos <-  tibble::as_tibble(datos_arcos$data) %>% select(endItem.id, startItem.id)

  # Los arcos tienen el id de los nodos que tocan, ahora agrego el dato "var" del paso anterior.
  datos_arcos_completos <-   datos_arcos %>%
                             dplyr::filter(complete.cases(.)) %>%
                             dplyr::left_join(., papelitos, by = join_by(endItem.id == id)) %>%
                             dplyr::left_join(., papelitos, by = join_by(startItem.id == id)) %>%
                             dplyr::select(startItem.id, var.y, endItem.id, var.x) %>%
                             dplyr::rename(end_n = var.x, start_n = var.y)

  miro_datos <-  list(nodos = papelitos,
                      arcos = datos_arcos_completos,
                      marcos = datos_marcos)
  return(miro_datos)
}


#' Assemble the DAG as recovered from Miro sticky notes and arcs
#'
#' This function builds a formal DAG sing DOT language and dagitty
#' Using dagitty it produces a list of implied conditional independence conditions.
#' Using ggdag pprodces a graphical representation of the DAG.
#'
#' @param nodos Node data as recovered from Miro board
#' @param arcos Arc data as recovered from Miro board
#' @return a list holding a ggdag plot, text list of
#' implied conditional independenceof nodes, arcs and frames attributes,
#' and the DAG itself as interpreted by dagitty.
#' @export
prepara_DAG <- function(nodos, arcos)
{
  # Componentes del DAG
  var_nombradas <-  nodos$var[nodos$var != "-"]
  arcos_validos <- arcos[(arcos$start_n != "-") &
                           (arcos$end_n != "-") &
                           (!is.na(arcos$start_n)) &
                           (!is.na(arcos$end_n)),]
  dag_cafe <- 'dag {bb="0,0,1,1"\n'
  dag_arcos <- paste0(arcos_validos$start_n, " -> ",
                      arcos_validos$end_n,
                      "\n", collapse = "")


  # Armado de DAG: Considero sólo las variables conectadas
  dag_cafe_efectivo <- paste0(dag_cafe, dag_arcos, "}")
  dag_efectivo <- dagitty::dagitty(dag_cafe_efectivo)
  coord_dag_efectivo <- dagitty::coordinates(dagitty::graphLayout(dag_efectivo))
  dagitty::coordinates(dag_efectivo) <- coord_dag_efectivo

  # Grafico el DAG con ggdag, más estético
  dag_efectivo_gg <- ggdag::tidy_dagitty(dag_efectivo)

  dag_graf <- dag_efectivo_gg %>%
              ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
              ggdag::geom_dag_point(aes(color = name), position = "jitter", size = 2) +
              ggdag::geom_dag_edges_arc(curvature = c(0, .5), edge_color = "gray50") +
              ggdag::geom_dag_text_repel(aes(label=name, fill = name, color = name),
                                  size = 3, force = 2) +
              ggplot2::theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),
                    legend.position = "")

  # DAG: Independencia condicional implicada
  ind_cond <- dagitty::impliedConditionalIndependencies(dag_cafe_efectivo)

  # Independencia condicional implicada en formato gráfico (Latex)
  for (i in ind_cond)
  {
    if (identical(i, ind_cond[[1]]))
    {
      if (length(i$Z) > 0 )
      {
        ind_cond_t <- tibble::tibble(linea = paste0("$\\newcommand{\\indep}{\\perp \\\\!\\\\!\\\\! \\perp}$",
                                    "\n$",
                                    gsub("_", "~", i$X),
                                    "~ \\indep ~",
                                    gsub("_", "~", i$Y),
                                    "~~ |~",
                                    gsub("_", "~", paste0(i$Z, collapse = ",~")),
                                    "$\n ", collapse = " "))
      } else
      {
        ind_cond_t <- tibble::tibble(linea = paste0("$\\newcommand{\\indep}{\\perp \\\\!\\\\!\\\\! \\perp}$",
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
          add_row(tibble::tibble(linea = paste0("$",
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
          add_row(tibble::tibble(linea = paste0("$",
                                 gsub("_", "~", i$X),
                                 "~ \\indep ~",
                                 gsub("_", "~", i$Y),
                                 "$\n ", collapse = " ")))
      }
    }
  }
  # Guarda documento **DNE** en disco
  DAG_datos <-  list(gg_dag = dag_graf,
                     indepCond = ind_cond_t,
                     dag = dag_efectivo)
  return(DAG_datos)
}


#' This funnction builds a representation of the Bayesian
#' Network recovered from Miro in a DNE file format that
#' Netica and other Bayesian Network application can reed.
#'
#' @param datos_marcos Frame data as recovered from Miro board
#' @param papelitos Node data as recovered from Miro board
#' @param arcos Arc data as recovered from Miro board
#' @return a string that contains the whole DNE document
#'
#' @export
red2DNE <- function(datos_marcos, papelitos, arcos)
{
  ## Transfiere datos a Netica
  arcos_validos <- arcos[(arcos$start_n != "-") &
                           (arcos$end_n != "-") &
                           (!is.na(arcos$start_n)) &
                           (!is.na(arcos$end_n)),]

  nodos_conectados <- unique(c(arcos_validos$start_n, arcos_validos$end_n))
  nodos <- sapply(nodos_conectados, function(key) list(padres = character()), simplify=F)

  # Agrega color del grupo y coordenadas a los nodos y prepara la traslación al origen común.
  for(n in names(nodos))
  {
    marco_id <- papelitos$frame_id[papelitos$var == n]
    if (!is.na(marco_id))
    {
      grupo <- strsplit(datos_marcos$data.title[datos_marcos$id == marco_id], " ")[[1]][1]
      color <- datos_marcos$style.fillColor[datos_marcos$id == marco_id]
      off_x <- datos_marcos$position.x[datos_marcos$id == marco_id]
      off_y <- datos_marcos$position.y[datos_marcos$id == marco_id]
    } else
    {
      grupo <- NA
      color <- NA
      off_x <- -300
      off_y <- 400
    }

    nodos[[n]]["x"] <- papelitos$x[papelitos$var == n] + off_x
    nodos[[n]]["y"] <- papelitos$y[papelitos$var == n] + off_y
    nodos[[n]]["color"] <- color
    nodos[[n]]["grupo"] <- grupo
  }

  # Valores de escalamiento
  min_x <- min(datos_marcos$position.x, sapply(nodos, function(d) d$x))
  min_y <- min(datos_marcos$position.y, sapply(nodos, function(d) d$y))
  max_x <- max(datos_marcos$position.x, sapply(nodos, function(d) d$x))
  max_y <- max(datos_marcos$position.y, sapply(nodos, function(d) d$y))
  esc_x  <- (max_x - min_x) / 1500
  esc_y  <- (max_y - min_y) / 800

  # Traslada y escala la posición de los nodos
  for(n in names(nodos))
  {
    nodos[[n]]["x"] <- as.integer((nodos[[n]][["x"]]  - min_x) / esc_x)
    nodos[[n]]["y"] <- as.integer((nodos[[n]][["y"]]  - min_y) / esc_y)
  }

  # Construye la lista de "padres" de cada nodo
  for (a_i in 1:length(arcos_validos$end_n))
  {
    nodo <- arcos_validos$end_n[[a_i]]
    padre <- arcos_validos$start_n[[a_i]]
    nodos[[nodo]]$padres = append(nodos[[nodo]]$padres, padre)
  }


  # Encabezado del archivo DNE para Netica
  nombre_red <- "Red_Produccion_Cafe"
  epoch_time <- as.integer(Sys.time())  # tiempo en segundos desde 01-01-1970

  # Formación de grupos
  grupos <- datos_marcos %>%
    dplyr::select(grupo = data.title, color = style.fillColor) %>%
    dplyr::mutate(grupo = stringi::stri_trans_general(
                                      stringr::str_extract(grupo, ".*?(?= )"),"Latin-ASCII"),
                  color = stringr::str_replace(color, "#", "Color = 0x0")) %>%
    dplyr::mutate(grupo = str_sub(grupo))

  # Lista de nodos por grupo
  grupo_nodos = list()
  for (n in names(nodos))
  {
    g_i = stringi::stri_trans_general(nodos[[n]]$grupo, "Latin-ASCII")
    if (!is.na(g_i))
    {
      if (n == names(nodos)[1])
      {
        grupo_nodos[[g_i]] = n
      } else
      {
        grupo_nodos[[g_i]] = c(grupo_nodos[[g_i]], n)
      }
    }
  }

  # Construye las líneas NodeSet" que usa Netica para colorear nodos por grupos
  for (g in grupos$grupo)
  {
    if (!is.na(g))
    {
      color = grupos$color[grupos$grupo == g]
      g_temp <- paste0("    NodeSet ", g, " {", color, ";};\n", collapse = "")
      if (g == grupos$grupo[1])
      {
        grupos_dne <- g_temp
      } else
      {
        grupos_dne <- c(grupos_dne, g_temp)
      }
    }
  }

  # Arma la sección general del doc DNE (inclye NodeSets)
  doc_dne <- paste0("// ~->[DNET-1]->~\n",
                    "\n",
                    "// File created by EquihuaM at InstEco_MX ",
                    "using Netica 6.05 on Jun 03, 2023 at 03:09:37 UTC.\n",
                    "\n",
                    "bnet ", nombre_red, " {\n",
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
                    paste0(grupos_dne, collapse = ""),
                    "    PrinterSetting A {\n",
                    "        margins = (1270, 1270, 1270, 1270);\n",
                    "        };\n    };\n\n", collapse = "")

  # Prepara la sección que define a los nodos individualmente
  i <- 0
  dne_nodos <- character()
  for (nodo in names(nodos))
  {
    i <- i + 1
    dne_nodos <- c(dne_nodos, paste0("node ", nodo, " {\n",
                                     "    discrete = TRUE;\n",
                                     "    states = (A, B, C);\n",
                                     "    kind = NATURE;\n",
                                     "    chance = CHANCE;\n",
                                     "    parents = (",
                                     paste0(nodos[[nodo]]$padres, collapse = "," ),");\n",
                                     "    whenchanged = ", epoch_time, ";\n",
                                     "    visual V1 {\n",
                                     "        center = (", nodos[[nodo]]$x, ", ",
                                     nodos[[nodo]]$y, ");\n",
                                     "        height = ", i, ";\n",
                                     "        };\n",
                                     "    };\n", collapse = ""))
  }

  dne_gr_nodos <- tibble::tibble(grupo = names(grupo_nodos),
                         nodos = as.character(
                                    sapply(grupo_nodos,
                                           function(nodos) paste0(nodos, collapse = ","))))

  # Para finalizar el documento DNE hay qe agregar la lista de agrupamientos
  dne_nodos <- paste0(dne_nodos, collapse = "\n")
  doc_dne_completo <- paste0(doc_dne, dne_nodos,
                             paste0("NodeSet ", dne_gr_nodos$grupo,
                                    " {Nodes = (",
                                    dne_gr_nodos$nodos, ");};\n",
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


#' This function does a quick check on the nmeric consistency
#' of the network attributes as interpreted from the data
#' found in the Miro board.
#'
#' @param papelitos Node data as recovered from Miro board
#' @param var Node data as recovered from Miro board.
#' @return tibble::tibble with numbers sumirizing the network structure.
#' @export
miro_validar <- function(papelitos, arcos)
{
  num_nodos <- length(papelitos$id)
  arcos_validos <- arcos[(arcos$start_n != "-") &
                           (arcos$end_n != "-") &
                           (!is.na(arcos$start_n)) &
                           (!is.na(arcos$end_n)),]

  nodos_conectados <- unique(c(arcos_validos$start_n, arcos_validos$end_n))

  num_nodos_conectados <- length(nodos_conectados)
  nodos_sin_var <- length(papelitos$var[papelitos$var == "-"])
  nodos_nombre_repetido <- num_nodos - nodos_sin_var -
                           length(unique(papelitos$var[papelitos$var != "-"]))
  num_arcos <- length(arcos$endItem.id)
  num_arcos_validos <- length(arcos_validos$endItem.id)
  arcos_sueltos <- length(arcos$endItem.id) -
                          length(arcos$endItem.id)
  arcos_duplicados <- length(arcos_validos$start_n[duplicated(arcos_validos)])

  validacion <- tibble::tibble(num_nodos, num_nodos_conectados, nodos_sin_var,
                       nodos_nombre_repetido, num_arcos, num_arcos_validos,
                       arcos_sueltos, arcos_duplicados)
  return(validacion)
}
