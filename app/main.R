box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, 
        renderText, tags, textOutput, tagList,HTML],
  shinythemes[shinytheme]
  )

box::use(
  app/view/a_home,
)

#-------------------
# UI
#-------------------


#' @export
ui <- function(id) {
  ns <- NS(id)

tagList(
  
navbarPage(
  theme = shinytheme("yeti"),
  "IBDB",
  tabPanel ("home", a_home$ui(ns("home"))),
  tabPanel ("Explore"),
  tabPanel ("Download"),
  tabPanel ("Documentation")
  ),

tags$footer(HTML(footerHTML()))

  )
}

footerHTML <- function() {
  "
    <footer class='footer'>
      <div style='position: fixed;
      bottom: 0;width: 100%;height:60px'>
      <div class='footer-copyright text-center py-3'><span style='color:black'>LiverDB © 2022 Copyright:</span>
        <a href='http://heartlncrna.github.io/' target='_blank'>heartlncrna</a> 
        <span>&nbsp</span>
        <a href='https://github.com/RVel37/IBDB' target='_blank'> 
          <img src='GitHub-Mark-Light-64px.png' height='20'>
        </a>
      </div>
    </footer>
  "
}


#--------------------
# SERVER
#--------------------

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderText("Hello!")
  })
}
