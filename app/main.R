# app/main.R

box::use(
  shiny[bootstrapPage, moduleServer, NS,
        tags,...],
)

box::use(
  #logic folder
  app/logic/data,
  app/logic/ui_globals,
  app/logic/utils,
  
  #view folder
  app/view/memory,
  app/view/UI,
  app/view/DEGresults,
  app/view/downloads,
  app/view/plots,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  bootstrapPage(
  UI$ui(ns("UI")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

      #raw data
    app_data <- readRDS("app/data/app_data.rds")
    
    memory$mem_server("memory")
    DEGresults$server("DEGs")
  })
}