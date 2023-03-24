box::use(
  shiny[bootstrapPage, moduleServer, NS, tags,...],
)

box::use(
  #view folder
  app/view/ui,
  app/view/server,
  #logic folder
  app/logic/ui_globals,
  app/logic/utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    ui$ui(ns("ui"))
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  #data
  deg_contrasts <- readRDS("app/data/deg_contrasts.rds", as.data.table = TRUE)
  degs <- readRDS("app/data/degs.rds",as.data.table=TRUE)
  eres <- readRDS("app/data/eres.rds",as.data.table=TRUE)
  exps <- readRDS("app/data/exps.rds",as.data.table=TRUE)
  #server
  server$server("server",
                deg_contrasts, degs, eres, exps)
  })
}
