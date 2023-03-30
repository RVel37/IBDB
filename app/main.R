box::use(
  shiny[navbarPage, tabPanel, moduleServer, NS, 
        renderText, tags, textOutput, tagList, HTML],
  shinythemes[shinytheme]
  )

box::use(
  app/view/a_home,
  app/view/b_explore,
  app/view/c_download,
  app/view/d_documentation,
  
  app/logic/main_utils[footerHTML],
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
  tabPanel ("Home", a_home$ui(ns("home"))),
  tabPanel ("Explore", b_explore$ui(ns("explore"))),
  tabPanel ("Download",c_download$ui(ns("download"))),
  tabPanel ("Documentation",d_documentation$ui(ns("documentation")))
  ),

tags$footer(HTML(footerHTML()))

  )
}


#--------------------
# SERVER
#--------------------

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    b_explore$server("explore")
    
  })
}
