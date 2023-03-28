box::use(
  shiny[navbarPage, tabPanel, moduleServer, 
        NS, renderText, tags, textOutput],
  shinythemes[shinytheme]
  )


#' @export
ui <- function(id) {
  ns <- NS(id)
  
navbarPage(
  theme = shinytheme("slate"),
  "IBDB",
  tabPanel ("home", "hi"),
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
