box::use(
  shiny[
    NS, tabPanel, icon, fluidPage, fluidRow, column, selectInput, dataTableOutput,
    moduleServer, renderTable, h2, h3, hr, p, renderPlot,reactive,renderDataTable],
  DT[renderDT],
  shinipsum[random_table,]
)

box::use(
  app / logic / b_explore_utils[ExplorePageContents]
)



#' @export
ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Explore",
    id = "explore-tab",
    icon = icon("table"),
    column(
      4,
      fluidRow(
        h2("Explore results"), hr(),
        column(
          6,
          # GSE drop down
          selectInput("GSE_dropdown", "Study",
            choices = c("GSE1", "GSE2")),
          p("view study [link] on GEO."),
        ),
        column(
          6,
          # contrasts drop down
          selectInput("cont_dropdown", "Contrast (Numerator vs. Denominator)",
            choices = c("Col1","Col2","Col3")),
         
        ), hr(),
      ), hr(),
      
     h3("Results Table"),
     dataTableOutput("selected_data")
      
    ), # end of left column

    column(
      8,
      
    ) # end of right column 
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
#generate sample data
    GSE1 <- random_table(nrow=10,ncol=3)
    GSE2 <- random_table(nrow=10,ncol=3)
    appdata <- list(GSE1,GSE2)
      
#table -- LHS
    selected_df <- reactive({
      matrix_choice <- input$GSE_dropdown
      col_choice <- match(input$cont_dropdown, c("Col1", "Col2", "Col3"))
      data_list[[matrix_choice]][, col_choice, drop = FALSE]
    })
    
   #render data table of user input
    output$table <- renderDT({
      data.frame(selected_df())
    })
    
    #render selected data
    output$selected_data <- renderDataTable({
      selected_df()
    })
  
#plots -- RHS
    
  
    })
}
