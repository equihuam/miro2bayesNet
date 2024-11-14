queryMiro <- function(board_id, object = "boards/", item_set, item_type,
                      item_id = "", page = "", miroCreds)
{
  page <- ""
  url_miro <- "https://api.miro.com/v2/"

  if(item_id == "")
  {
    sndURL <- paste0(url_miro, object, board_id, item_set)

    ifelse (item_set == "/items",
            qryStr  <- list(limit = "50", type = item_type),
            qryStr  <- list(limit = "50"))
    if (page != "") qryStr[["cursor"]] <-  page

    print(sndURL)
    print(qryStr)

    response <- httr::VERB("GET", sndURL,
                       httr::add_headers('authorization' = miroCreds),
                       query = qryStr,
                       httr::content_type("application/octet-stream"),
                       httr::accept("application/json"))

  } else {
      sndURL <- paste0(url_miro, object, board_id, "/items/", item_id, "/tags")
      response <- httr::VERB("GET", sndURL,
                             httr::add_headers('authorization' = miroCreds),
                             httr::content_type("application/octet-stream"),
                             httr::accept("application/json"))
    }


  miroQr_Data <- jsonlite::fromJSON(httr::content(response, "text",
                                                  encoding = "utf-8"),
                                    flatten = TRUE)
  return(miroQr_Data)
}

servMiro <- "miro"
user <-  "miguel-token"
credentials <- keyring::key_get(service = servMiro, username = user)
credentials <-  paste("Bearer", credentials)


testData <- queryMiro(board_id = "uXjVMkg60Fw=",
                      object = "boards/",
                      item_set = "/items",
                      item_type = "frame",
                      miroCreds = credentials)

testData2 <- queryMiro(board_id = "uXjVMkg60Fw=",
                      object = "boards/",
                      item_set = "/items",
                      item_type = "sticky_note",
                      miroCreds = credentials)

testData3 <- queryMiro(board_id = "uXjVMkg60Fw=",
                       object = "boards/",
                       item_set = "/connectors",
                       miroCreds = credentials)

pag1 <- testData3$cursor
testData3a <- queryMiro(board_id = "uXjVMkg60Fw=",
                       object = "boards/",
                       item_set = "/connectors",
                       miroCreds = credentials,
                       page = pag1)


testData4 <- queryMiro(board_id = "uXjVMkg60Fw=",
                       object = "boards/",
                       item_set = "/items/",
                       item_id = "3458764564033387583",
                       miroCreds = credentials)

