box::use(
  shiny[...],
  DT[renderDT,DTOutput],
  dplyr[filter],
  shinycssloaders[withSpinner],
  shinipsum[...],
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
          #GeneTable_panel
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
          #end of GeneTable_panel
        ),
        column(
          width = 8,
          #outputpanel_tabset
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
                    h4("Volcano plot"),
                    hr(),
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotOutput("volcano_plot", height = "600px"),
                  )
                )
              ),

            )
          )
          #end of outputpanel_tabset
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
