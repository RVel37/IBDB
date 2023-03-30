box::use(
  shiny[NS,tabPanel,icon,fluidPage,fluidRow,column,selectInput,tableOutput,
        moduleServer,renderTable]
)

box::use(
  app/logic/b_explore_utils[ExplorePageContents]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
tabPanel(
    title = "Explore",
    id = "explore-tab",
    icon = icon('table'))
  
fluidPage(
  fluidRow(
    column(width = 4),
    selectInput("dropdown1","Study",
                choices = c("GSE112057","GSE123141","GSE83687"),
    tableOutput("table1")),
    
    
    column(width = 8)
  )
)
}

#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    output$table <- renderTable({
      #blank table
    })
    
  })  
}