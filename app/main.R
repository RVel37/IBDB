box::use(
  shiny[navbarPage, tabPanel, moduleServer, 
        NS, renderText, tags, textOutput],
  shinythemes[shinytheme]
  )

box::use(
  app/view/home,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
navbarPage(
  theme = shinytheme("yeti"),
  "IBDB",
  tabPanel ("home", home$ui(ns("home"))),
  tabPanel ("Explore"),
  tabPanel ("Download"),
  tabPanel ("Documentation")
  
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderText("Hello!")
  })
}
