box::use(
  shiny[fluidPage,fluidRow,column,tagList,h3,hr,selectInput,...],
  shinycssloaders[withSpinner],
  DT[DTOutput],
  shinipsum, echarts4r,
)

###LHS - contrasts table

#'@export
GeneTable_panel <- function() {
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Explore Results"),
             hr())
    ),
    fluidRow(
      column(
        6,
        # GSE drop-down
        selectInput(
          inputId = "selectStudy", 
          label = "Study",
          selected = "GSE1",
          choices = c("GSE1", "GSE2")),
        
          p("view study [link] on GEO."),
      ),  # uiOutput("studyLink") ### study link ###

      # contrasts drop-down
      column(
        width = 6,
        uiOutput("UIselectContrast")
      )
    ),
    hr(),
    
    fluidRow(
      column(
        width = 12,
       h3("Results Table"),
          p("IBD RNA-Seq DGE analysis results."),
        
        withSpinner(DTOutput("degTable")) #contrasts table
      )
    )
  )
}


### RHS - plots

#'@export
OutputPanel_tabset <- function() {
  
  column(
    width = 12,
    tabsetPanel(
      id = "expTabset",
      tabPanel(
        title = "Expression",
        icon=icon('chart-bar'),
        
        #expression_panel()

      ),
      tabPanel(
        title = "Volcano plot",
        icon=icon('mountain'),
        fluidRow(
          column(
            width = 6,
            hr(),
            h3("Volcano plot"),
            hr(),
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("volcano_plot", height = "600px")
          )
        )
      ),
      tabPanel(
        title = "Heatmap",
        icon=icon("burn"),
        #Heatmap_panel()
      
      ),
      tabPanel(
        title = "Pathway analysis",
        icon=icon("project-diagram"),
        #Enrich_panel()
        
      ),
      tabPanel(
        title = "Comparison",
        icon=icon("adjust"),
        #Upset_panel()
        
      )
    )
  )
}
