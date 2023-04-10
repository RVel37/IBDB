box::use(
  shiny[
    NS, tabPanel, icon, fluidPage, fluidRow, column, selectInput, dataTableOutput,
    moduleServer, renderTable, h2, h3, hr, p, renderPlot,reactive,renderDataTable,
    renderUI],
  DT[renderDT],
  dplyr[filter],
  shinipsum,
)

box::use(
  app/logic/b_explore_utils[GeneTable_panel, OutputPanel_tabset]
)


#'@export
ui <- function(id) {
  ns <- NS(id)

tabPanel(
    title = "Explore",
    id = "explore-tab",
   
    fluidPage(
      title = "Explore",
      fluidRow(
        column(
          width = 4,
          GeneTable_panel(),
        ),
        column(
          width = 8,
          OutputPanel_tabset(),
        )
      )
    ))
}


#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
print("explore module server works")


# contrasts selector
    output$UIselectContrast <- renderUI({
      study <- input$selectStudy
      cont_labels <- deg_contrasts %>%
        dplyr::filter(study_id == study) %>% 
        unite("contrast", c("numerator", "denominator"), sep = " vs. ") %>% 
        pull(contrast)
      
      selectInput(
        inputId = "selectContrast",
        label = "Contrast (Numerator vs. Denominator)",
        selected = cont_labels[1],
        choices = cont_labels
      )
    })

# DEG results table
output$degTable <- DT::renderDT({
  random_DT(10,3)
})

output$volcano_plot <- renderPlot({
  random_ggplot()
})

    })
}
