#' List all Miro board
#'
#' This function recovers a list of all the boards available to the user
#' identified by provided credentials.
#'
#' @param servMiro Name of the credential service as defined in keyring setup
#' @param user User name as defined n keyring setup
#' @export
miroBoards <- function(servMiro = "miro", user)
{
  credentials <- keyring::key_get(service = servMiro, username = user)
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


#' Gets Miro data into R
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
#' @param board Data frame with id code and name of the Miro board to access
#' @return a list of nodes, arcs and frames attributes
#' @export
getMiro <- function (servMiro = "miro", user, board)
{
  # getMiro internal functions -------------
  cleanFun <- function(htmlString)
    {
      return(gsub("<.*?>", "", htmlString))
    }


  queryMiro <- function(board_id, object = "boards/", item_set, item_type,
                        item_id = "", page = "", miroCreds)
  {
    url_miro <- "https://api.miro.com/v2/"

    if(item_id == "")   # Get drawing Items on the board
    {
      sndURL <- paste0(url_miro, object, board_id, item_set)

      ifelse (item_set == "/items",
              qryStr  <- list(limit = "50", type = item_type),
              qryStr  <- list(limit = "50"))
      if (page != "") qryStr[["cursor"]] <-  page

      response <- httr::VERB("GET", sndURL,
                             httr::add_headers('authorization' = miroCreds),
                             query = qryStr,
                             httr::content_type("application/octet-stream"),
                             httr::accept("application/json"))

    } else {    # get tags attached to Sticky-notes
      sndURL <- paste0(url_miro, object, board_id, "/items/", item_id, "/tags")
      response <- httr::VERB("GET", sndURL,
                             httr::add_headers('authorization' = miroCreds),
                             httr::content_type("application/octet-stream"),
                             httr::accept("application/json"))
    }

    # Apply convert response to json for further processing
    miroQr_Data <- jsonlite::fromJSON(httr::content(response, "text",
                                                    encoding = "utf-8"),
                                      flatten = TRUE)
    return(miroQr_Data)

  }

  # getMiro main body --------------------------------------------------

  # Basic items to access Miro team space online
  if (!any(stringr::str_detect(names(board), "id")))
  {
    board <- tibble::tibble(id = board, name = paste0("Miro: ", name))
  }

  credentials <- keyring::key_get(service = servMiro, username = user)
  credentials <-  paste("Bearer", credentials)

  # Recover frames present in selected board
  frames_data <- queryMiro(board_id = board$id,
                           object = "boards/",
                           item_set = "/items",
                           item_type = "frame",
                           miroCreds = credentials)
  frames_data <- tibble::as_tibble(frames_data$data)

  # Get Miro data of the nodes provided by user in "Sticky notes"
  nodes_page <- queryMiro(board_id = board$id,
                          object = "boards/",
                          item_set = "/items",
                          item_type = "sticky_note",
                          miroCreds = credentials)

  num_nodes <-  nodes_page[c("size", "limit", "total")]

  # Document frame data
  if (any(stringr::str_detect(names(nodes_page$data), "parent.id")))
  {
    frame_id <- nodes_page$data$parent.id
  } else {
    frame_id <- NA
  }

  nodes_page$data$text <- unlist(lapply(nodes_page$data$data.content,
                                        cleanFun))

  nodesData <- tibble::tibble(id = nodes_page$data$id,
                              text = nodes_page$data$text,
                              color = nodes_page$data$style.fillColor,
                              frame_id = frame_id,
                              x = as.integer(nodes_page$data$position.x),
                              y = as.integer(nodes_page$data$position.y))

  # Process remaining sticky_notes pages available
  if (num_nodes$total > num_nodes$limit)
  {
      pages <- ceiling(num_nodes$total / num_nodes$limit) - 1
      for(i in (1:pages))
      {
        nodes_page <- queryMiro(board_id = board$id,
                                object = "boards/",
                                item_set = "/items",
                                item_type = "sticky_note",
                                miroCreds = credentials,
                                page = nodes_page$cursor)

        # Document frame data
        if (any(stringr::str_detect(names(nodes_page$data), "parent.id")))
        {
          frame_id <- nodes_page$data$parent.id
        } else {
          frame_id <- NA
        }

        nodes_page$data$text <- unlist(lapply(nodes_page$data$data.content,
                                              cleanFun))
        nodesData <-  tibble::tibble(id = nodes_page$data$id,
                                 text = nodes_page$data$text,
                                 color = nodes_page$data$style.fillColor,
                                 frame_id = frame_id,
                                 x = as.integer(nodes_page$data$position.x),
                                 y = as.integer(nodes_page$data$position.y)) %>%
                      bind_rows(nodesData)
      }
  }

  # Get single tag in sticky note, naming the variable associated to that node
  for (i in (1:length(nodesData$id)))
  {
    id <- nodesData$id[i]
    labels_page <- queryMiro(board_id = board$id,
                             object = "boards/",
                             item_set = "/items/",
                             item_id = id,
                             miroCreds = credentials)

    if (i == 1)
    {
      if (is.null(labels_page$tags$title))
      {
        tags <-  "-"
      } else
      {  tags <-  labels_page$tags$title}
    } else
    {
      if (is.null(labels_page$tags$title))
      {
        tags <-  c(tags, "-")
      } else
      {tags <-  c(tags, labels_page$tags$title) }
    }
  }

  nodesData <- nodesData %>% tibble::add_column(var = tags)
  nodesData <- nodesData[nodesData$text != "", ]

  # Arc data
  arcs_page <- queryMiro(board_id = board$id,
                         object = "boards/",
                         item_set = "/connectors",
                         miroCreds = credentials)

  arcs_data <-  tibble::as_tibble(arcs_page$data) %>%
                dplyr::select(endItem.id, startItem.id)
  num_arcs <-  arcs_page[c("size", "limit", "total")]

  # Process as many arc pages as available
  if(num_arcs$total > num_arcs$limit)
  {
    pages <- ceiling(num_arcs$total / num_arcs$limit) - 1
    for(i in (1:pages))
    {
      arcs_page <- queryMiro(board_id = board$id,
                             object = "boards/",
                             item_set = "/connectors",
                             miroCreds = credentials,
                             page = arcs_page$cursor)

      arcs_data <-  tibble::as_tibble(arcs_page$data) %>%
                    dplyr::select(endItem.id, startItem.id) %>%
                    dplyr::bind_rows(arcs_data)
    }
  }

  # Arcs have the ids of linked nodes. Adding "var" names from node data.
  arcs_data_raw <- arcs_data
  arcs_data_full <-   arcs_data %>%
                      dplyr::filter(complete.cases(.)) %>%
                      dplyr::left_join(., nodesData, by = dplyr::join_by(endItem.id == id)) %>%
                      dplyr::left_join(., nodesData, by = dplyr::join_by(startItem.id == id)) %>%
                      dplyr::select(startItem.id, var.y, endItem.id, var.x) %>%
                      dplyr::rename(end_n = var.x, start_n = var.y)

  miroData <- list(board = board$name,
                   nodes = nodesData,
                   arcs = arcs_data_full,
                   frames = frames_data)

  # Adding DAG and raw arc data
  miroData[["dag"]] <- miroDAG(miroData)
  miroData[["arcs_raw"]] <-arcs_data_raw
  return(miroData)
}


#' This function does a quick check on the numeric consistency
#' of the network attributes as interpreted from the data
#' found in the Miro board.
#'
#' @param miroData Data recovered from Miro board
#' @return tibble::tibble with numbers summarizing the network structure.
#' @export
miroValidation <- function(miroData)
{
  nodesData <- miroData$nodes
  arcs <- miroData$arcs
  dag <-  miroData$dag

  acyclic <- "Graph is not acyclic"
  if (dagitty::isAcyclic(dag$dag))
  {
    acyclic <- "Graph is acyclic"
  }

  num_nodes <- length(nodesData$id)
  valid_arcs <- arcs[(arcs$start_n != "-") &
                       (arcs$end_n != "-") &
                       (!is.na(arcs$start_n)) &
                       (!is.na(arcs$end_n)),]

  nodes_linked <- unique(c(valid_arcs$start_n, valid_arcs$end_n))

  num_nodes_linked <- length(nodes_linked)
  nodes_without_var <- length(nodesData$var[nodesData$var == "-"])
  repeated_node_names <- num_nodes - nodes_without_var -
    length(unique(nodesData$var[nodesData$var != "-"]))
  num_arcs <- length(arcs$endItem.id)
  num_valid_arcs <- length(valid_arcs$endItem.id)
  unlinked_arcs <- length(arcs$endItem.id) -
    length(arcs$endItem.id)
  duplicated_arcs <- length(valid_arcs$start_n[duplicated(valid_arcs)])

  check <- tibble::tibble(acyclic, num_nodes, num_nodes_linked,
                          nodes_without_var, repeated_node_names,
                          num_arcs, num_valid_arcs, unlinked_arcs,
                          duplicated_arcs)

  cat("Miro board origin:      ", miroData$board, "\n",
      "Is it a TRUE DAG?:      ", check$acyclic, "\n",
      "Number of sticky notes: ", check$num_nodes, "\n",
      "Nodes without var:      ", check$nodes_without_var, "\n",
      "Number of linked nodes: ", check$num_nodes_linked, "\n",
      "Duplicated nodes:       ", check$repeated_node_names, "\n",
      "Number of arcs:         ", check$num_arcs, "\n",
      "Well connected arcs:    ", check$num_valid_arcs, "\n",
      "Numb. Loose arcs:       ", check$unlinked_arcs, "\n",
      "Duplicated arcs:        ", check$duplicated_arcs, "\n",
      sep = "")
}


#' This funnction builds a representation of the Bayesian
#' Network recovered from Miro in a DNE file format that
#' Netica and other Bayesian Network application can reed.
#'
#' @param miroData List of data as recovered from the Miro board.
#' @return a string that contains the whole DNE document
#'
#' @export
miro2DNE <- function(miroData)
{
  frames_data <- miroData$frames
  nodesData <- miroData$nodes
  arcs <- miroData$arcs
  network_name <- miroData$board %>%
    stringi::stri_replace_all_regex(
               "\\s|\\:|\\;|\\,|\\.|\\-|\\(|\\)|\\{|\\}|\\[|\\]",
               "_") %>%
    stringi::stri_replace_all_regex("_{2,}", "_") %>%
    stringi::stri_trans_general(id = "Latin-ASCII") %>%
    stringi::stri_sub(from = 1, to = 30) %>%
    stringi::stri_trim_both("[_\\(\\)\\[\\]\\{\\]}]", negate = TRUE)

  ## DAG to DNE format. To be read by Netica (c) from Nosrsys, and other aplications.
  valid_arcs <- arcs[(arcs$start_n != "-") &
                     (arcs$end_n != "-") &
                     (!is.na(arcs$start_n)) &
                     (!is.na(arcs$end_n)),]

  nodes_linked <- unique(c(valid_arcs$start_n, valid_arcs$end_n))
  nodes_linked <- sapply(nodes_linked, function(key) list(parents = character()), simplify=F)

  # Get colors and relative positions from frame  data to display nodes on a common canvas.
  for(n in names(nodes_linked))
  {
    frame_id <- nodesData$frame_id[nodesData$var == n]
    print(frame_id)
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

    nodes_linked[[n]]["x"] <- nodesData$x[nodesData$var == n] + off_x
    nodes_linked[[n]]["y"] <- nodesData$y[nodesData$var == n] + off_y
    nodes_linked[[n]]["color"] <- color
    nodes_linked[[n]]["group"] <- group
  }

  # Scaling values to position nodes on common canvas.
  min_x <- min(frames_data$position.x, sapply(nodes_linked, function(d) d$x))
  min_y <- min(frames_data$position.y, sapply(nodes_linked, function(d) d$y))
  max_x <- max(frames_data$position.x, sapply(nodes_linked, function(d) d$x))
  max_y <- max(frames_data$position.y, sapply(nodes_linked, function(d) d$y))
  esc_x  <- (max_x - min_x) / 1500
  esc_y  <- (max_y - min_y) / 800
  if(esc_x == 0) {esc_x <- 1}
  if(esc_y == 0) {esc_y <- 1}

  # Node coordinates translated to a common canvas.
  for(n in names(nodes_linked))
  {
    nodes_linked[[n]]["x"] <- as.integer((nodes_linked[[n]][["x"]]  - min_x) / esc_x)
    nodes_linked[[n]]["y"] <- as.integer((nodes_linked[[n]][["y"]]  - min_y) / esc_y)
  }

  # Buid list of "parents" for each node.
  for (a_i in 1:length(valid_arcs$end_n))
  {
    node <- valid_arcs$end_n[[a_i]]
    parent <- valid_arcs$start_n[[a_i]]
    nodes_linked[[node]]$parents = append(nodes_linked[[node]]$parents, parent)
  }

  # Heading onf DNE file
  epoch_time <- as.integer(Sys.time())  # time in seconds since 01-01-1970

  # Specifying attributes for groups
  if (rlang::is_empty(frames_data))
  {
    groups_dne <- ""
    group_nodes = list()
  } else {
    groups <- frames_data %>%
      dplyr::select(group = data.title, color = style.fillColor) %>%
      dplyr::mutate(group = stringi::stri_trans_general(group, "Latin-ASCII"),
                    color = stringr::str_replace(color, "#", "Color = 0x0")) %>%
      dplyr::mutate(group = stringr::str_sub(group))

    # List of nodes in each group
    group_nodes = list()
    for (n in names(nodes_linked))
    {
      g_i = stringi::stri_trans_general(nodes_linked[[n]]$group, "Latin-ASCII")
      if (!is.na(g_i))
      {
        if (n == names(nodes_linked)[1])
        {
          group_nodes[[g_i]] = n
        } else
        {
          group_nodes[[g_i]] = c(group_nodes[[g_i]], n)
        }
      }
    }

    # "NodeSet" lists used to refer display attributes in Netica
    for (g in groups$group)
    {
      if (!is.na(g))
      {
        color = groups$color[groups$group == g]
        g_temp <- paste0("    NodeSet ", g, " {", color, ";};\n", collapse = "")
        if (g == groups$group[1])
        {
          groups_dne <- g_temp
        } else {
          groups_dne <- c(groups_dne, g_temp)
        }
      }
    }

  }

  # Builds the general sections of DNE document (including NodeSets)
  doc_dne <- paste0("// ~->[DNET-1]->~\n",
                    "\n",
                    "// File created by miro2bayes",
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

  # Builds section defining each node
  i <- 0
  dne_nodes <- character()
  for (nodo in names(nodes_linked))
  {
    i <- i + 1
    dne_nodes <- c(dne_nodes, paste0("node ", nodo, " {\n",
                                     "    discrete = TRUE;\n",
                                     "    states = (A, B, C);\n",
                                     "    kind = NATURE;\n",
                                     "    chance = CHANCE;\n",
                                     "    parents = (",
                                     paste0(nodes_linked[[nodo]]$parents, collapse = "," ),");\n",
                                     "    whenchanged = ", epoch_time, ";\n",
                                     "    visual V1 {\n",
                                     "        center = (", nodes_linked[[nodo]]$x, ", ",
                                     nodes_linked[[nodo]]$y, ");\n",
                                     "        height = ", i, ";\n",
                                     "        };\n",
                                     "    };\n", collapse = ""))
  }

  dne_nodes <- paste0(dne_nodes, collapse = "\n")

  # Final component is agrouping list of nodes
  if (!purrr::is_empty(group_nodes))
  {
    dne_gr_nodes <- tibble::tibble(group = names(group_nodes),
                                   nodes = as.character(sapply(group_nodes,
                                           function(nodes_grp) paste0(nodes_grp, collapse = ","))))
    doc_dne_completo <- paste0(doc_dne, dne_nodes,
                               paste0("NodeSet ", dne_gr_nodes$group,
                                      " {Nodes = (",
                                      dne_gr_nodes$nodes, ");};\n",
                                      collapse = "")
                               , "};", collapse = "")

  } else {
    doc_dne_completo <- paste0(doc_dne, dne_nodes, "};", collapse = "")
  }
  return(doc_dne_completo)
}


#' This function extracts the subset of the implied conditional
#' independences that containe an specified node name.
#'
#' @param miroData List of data as recovered from the Miro board.
#' @param var target variable to filter the conditional independence list.
#' @return string containing the subset of statements of conditional independence.
#' @export
cond_indepOnvar <-  function (miroData, var)
{
  indeps <- miroData$dag$indepCond
  var <-  gsub("_", "~", var)

  sub_in_cond <- indeps %>%
                 dplyr::select(line) %>%
                 dplyr::filter(grepl(paste0(var, "(?=.*?~~[|])"),
                                     indeps$line, perl = TRUE)) %>%
                 dplyr::mutate(line = stringr::str_trim(line)) %>%
                 dplyr::mutate(line = stringr::str_replace(line, "\\$",
                                                           "$\\\\color\\{blue\\} \\{")) %>%
                 dplyr::mutate(line = stringr::str_replace(line, "\\|",
                                                           "\\} \\|"))
  sub_in_cond_txt <- paste0(sub_in_cond$line, sep= "\n", collapse = "\n")

  sub_in_cond <- paste0("---\ntitle: ", '"', "Conditional Independences", '"\n',
                      "output: html_document\n",
                      "date: ", '"', Sys.Date(), '"\n',
                      "---\n","## _", var, "_\n",
                      "$$\n\\newcommand{\\indep}{\\perp\\!\\!\\!\\perp}\n$$\n\n",
                      sub_in_cond_txt, collapse = "")
  file_ic_var_md <- paste0("IC_", var, ".rmd")
  file_ic_var_html <- paste0("IC_", var, ".html")
  write(sub_in_cond, file_ic_var_md)
  rmarkdown::render(file_ic_var_md, output_format = "html_document")
  rstudioapi::viewer(file_ic_var_html)
}


#' This function feeds the data from Miro into a bnlearn network.
#'
#' @param variables Data on Nodes as recovered from Miro board
#' @param arcs Data on arcs links as recovered from Miro board.
#' @return tibble::tibble with numbers sumirizing the network structure.
#' @export
miro2bnlearn <- function(miroData)
{
  nodes <- miroData$nodes
  arcs <- miroData$arcs
  frames <- miroData$frames

  # TODO Construye las líneas NodeSet" que usa Netica para colorear nodes por groups
  # for (g in groups$group)
  # {
  #   if (!is.na(g))
  #   {
  #     color = groups$color[groups$group == g]
  #     g_temp <- paste0("    NodeSet ", g, " {", color, ";};\n", collapse = "")
  #     if (g == groups$group[1])
  #     {
  #       groups_dne <- g_temp
  #     } else
  #     {
  #       groups_dne <- c(groups_dne, g_temp)
  #     }
  #   }
  # }

  nodes_active <- arcs[(arcs$start_n != "-") &
                                (arcs$end_n != "-") &
                                (!is.na(arcs$start_n)) &
                                (!is.na(arcs$end_n)), ]

  nodeset <- unique(c(nodes_active$start_n, nodes_active$end_n))

  net_base = bnlearn::empty.graph(nodeset)

  bnlearn::arcs(net_base, check.cycles = FALSE) <- as.matrix(nodes_active[, c("start_n", "end_n")])

  return(net_base)
}


#' Assemble the DAG as recovered from Miro sticky notes and arcs
#'
#' This is an internal function to build a formal DAG using DOT language
#' and dagitty. The processing includes a list of implied conditional
#' independence conditions. Using ggdag produces a graphical representation
#' of the DAG, also included in the returned list of objects.
#'
#' @param miroData List of data as recovered from the Miro board.
#' @return a list holding a ggdag plot, text list of
#' implied conditional independence of nodes, arcs and frames attributes,
#' and the DAG itself as interpreted by dagitty plot byy ggdag.
#' @noRd
miroDAG <- function(miroData)
{
  nodes <- miroData$nodes
  arcs <- miroData$arcs

  # DAG components
  valid_arcs <- arcs[(arcs$start_n != "-") &
                       (arcs$end_n != "-") &
                       (!is.na(arcs$start_n)) &
                       (!is.na(arcs$end_n)),]
  dag_head <- 'dag {bb="0,0,1,1"\n'
  dag_arcs <- paste0(valid_arcs$start_n, " -> ",
                     valid_arcs$end_n,
                     "\n", collapse = "")

  # DAG building: only considering linked variables
  dag_str_effective <- paste0(dag_head, dag_arcs, "}")
  dag_effective <- dagitty::dagitty(dag_str_effective)
  coord_dag_effective<- dagitty::coordinates(dagitty::graphLayout(dag_effective))
  dagitty::coordinates(dag_effective) <- coord_dag_effective

  # DAG protting with "ggdag" (better presentation than plain dagitty)
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

  # Implied conditions independence relations
  ind_cond <- dagitty::impliedConditionalIndependencies(dag_str_effective)

  # Display implied conditionsl independence in graphical mathematical format (Latex)
  ind_cond_t <- tibble::tibble()
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
      } else {
        ind_cond_t <- ind_cond_t %>%
          tibble::add_row(tibble::tibble(line = paste0("$",
                                                       gsub("_", "~", i$X),
                                                       "~ \\indep ~",
                                                       gsub("_", "~", i$Y),
                                                       "$\n ", collapse = " ")))
      }
    }
  }

  DAG_datos <-  list(board = miroData$board,
                     gg_dag = dag_graph,
                     indepCond = ind_cond_t,
                     dag = dag_effective)
  return(DAG_datos)
}


