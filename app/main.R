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
  app/view/layout,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  bootstrapPage(
  layout$ui(ns("layout")),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

     data <- readRDS("app/app_data.rds")
    
    memory$server("memory")
    layout$server("layout")
  })
}