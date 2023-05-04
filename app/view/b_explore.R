box::use(
  shiny[...],
  DT[renderDT,DTOutput],
  dplyr[filter],
  shinycssloaders[withSpinner],
  shinipsum[...],
)
box::use(
<<<<<<< Updated upstream
  app/logic/b_explore_utils[get_random_ggplot,
                            helpButton,
                            makeHeaders],
=======
  app/logic/b_explore_utils[
    get_exp_plotly, get_enrich_plot, get_upset_plot, get_volcano, get_heatmap,
    helpButton, makeHeaders,
  ],
  app/logic/db_utils,
>>>>>>> Stashed changes
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
#-------------------------------------------------------
# LEFT HAND SIDE (gene table panels)
#-------------------------------------------------------
    tagList(
      fluidRow(
        column(width = 12,
          h3("Explore Results"),
          hr())
            ),
            fluidRow(
              column(
                6,
# GSE drop-down -----------------------------
                selectInput(
                  inputId = ns("selectStudy"),
                  label = "Study",
                  selected = "line",
                  choices = c("line", "bin2d", "contour", "density")),

              ),  #here will go the uioutput (study link)

# study contrasts drop-down ------------------
              column(
                width = 6,
                selectInput(
                  inputId = ns("selectContrast"),
                  label = "Contrast",
                  selected = "Cont1",
                  choices = c("Cont1", "Cont2")),
              )
            ),
            hr(),

            fluidRow(
              column(
                width = 12,

# Results table ----------------------------
                h3("Results Table"),
                p("IBD RNA-Seq DEG analysis results."),
                withSpinner(DTOutput(ns("degTable")))
              )
            )
          )
        ),
#-------------------------------------------------------
# RIGHT HAND SIDE (output panel tabs)
#-------------------------------------------------------
        column(
          width = 8,
          column(
            width = 12,
            tabsetPanel(
# Expression plot -------------------------
              id = "expTabset",
              tabPanel(
                title = "Expression",
                icon=icon('chart-bar'),
                selectInput(
                  inputId = ns("selectNormExp"),
                  label = "Normalization",
                  selected = "CPM",
                  choices = c("CPM", "TPM", "RPKM")
                ),
                makeHeaders(
                  title = "Gene counts ",
                  message=paste0("Gene count plots for samples in the selected study.")
                ),
                hr(),
                uiOutput(ns("expHtml")),
                plotOutput(ns("expPlot"))
              ),
# Volcano plot----------------------------
              tabPanel(
                title = "Volcano plot",
                icon=icon('mountain'),
                fluidRow(
                  column(
                    width = 6,
                    hr(),
                    makeHeaders(
                      title = "Volcano plot ",
                      message=paste0("Volcano plot showing the differential gene expression results.")
                    ),
                    hr()
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotOutput(ns("volcanoPlot"), height = "600px")
                  )
                )
              ),
# Heatmap ------------------------------
              tabPanel(
                title = "Heatmap",
                icon=icon("burn"),
                #Heatmap_panel()
                fluidRow(
                  column(
                    width = 6,
                    hr(),
                    makeHeaders(
                      title = "Heatmap ",
                      message = paste0("Heatmap of top DEG count plots for samples in the selected study.")
                    ),
                    hr()
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    selectInput(
                      inputId = ns("selectNormHeatmap"),
                      label = "Normalization",
                      selected = "CPM",
                      choices = c("CPM", "TPM", "RPKM")
                    ),
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotOutput(ns("heatmap"),height = "500px")

                  )
                ),
              br(),
              ),
# Pathway analysis ------------------------------
              tabPanel(
                title = "Pathway analysis",
                icon=icon("project-diagram"),
                #Enrich_panel()
                fluidRow(
                  column(
                    width = 6,
                    hr(),
                    makeHeaders(
                      title = "KEGG enrichment ",
                      message=paste0("Heatmap of top hits from KEGG pathway enrichment (via 'enrichr' web service) in over- and under-expressed genes.")
                    ),
                    hr()
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    selectInput(
                      inputId = "selectEM",
                      choices = c("Combined.Score", "Odds.Ratio", "Padj (-log10)"),
                      selected = "Combined.Score",
                      label = "Enrichment Metric"
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    uiOutput(ns("enrichHtml")),
                    plotOutput(ns("enrichPlot"))
                    )
                  )
                ),
                br(),
              ),
# Comparison ------------------------------
              tabPanel(
                title = "Comparison",
                icon=icon("adjust"),
                #Upset_panel()
                fluidRow(
                  column(
                    width = 6,
                    hr(),
                    makeHeaders(
                      title = "DEG comparison ",
                      message=paste0("UpSet plot comparing over- and under-expressed genes between studies.")
                    ),
                    hr()
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    selectInput(
                      inputId = "upsetSelect",
                      choices = c("Over-expressed", "Under-expressed"),
                      selected = "Over-expressed",
                      label = "DEG type"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    uiOutput(ns("upsetHtml")),
                    plotOutput(ns("upsetPlot"))
                  )
                ),
                br()
              )
            ))
        ))))
}



#-------------------------------------------------------
# SERVER
#-------------------------------------------------------

#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # DEG results table
    output$degTable <- DT::renderDT({
      random_DT(10,3)
    })


#-------------------------------------------------------
# plots for each tab
#-------------------------------------------------------

#----------------------- EXPRESSION PLOT

    output$expPlot <- renderPlot({
      get_random_ggplot(input$selectStudy)
    })

<<<<<<< Updated upstream
# NOTE -- can later be converted to:
# create_exp_plot(input$selectStudy, input$selectContrast, input$selectNorm)

    output$expHtml <- renderUI({
      string <- paste0(
        input$selectStudy, input$selectContrast, input$selectNormExp
      )
      string
    })

#----------------------- VOLCANO PLOT

    output$volcanoPlot <- renderPlot({
      get_random_ggplot(input$selectStudy)
    })
=======
#----------------------- VOLCANO PLOT

    output$volcanoPlot <- renderPlot({
      req(input$selectStudy, input$selectContrast, current_gene())
>>>>>>> Stashed changes

      get_volcano(input$selectStudy, input$selectContrast, current_gene())
    })

#----------------------- HEATMAP

    output$heatmap <- renderPlot({
<<<<<<< Updated upstream
      get_random_ggplot(input$selectStudy)
    })
=======
      req(input$selectStudy, input$selectContrast, input$selectNormHeatmap)
>>>>>>> Stashed changes

      get_heatmap(input$selectStudy, input$selectContrast, input$selectNormHeatmap)
    })

#----------------------- PATHWAY ANALYSIS

    output$enrichPlot <- renderPlot({
      get_random_ggplot(input$selectStudy)
    })

    output$heatmapHtml <- renderUI({
      string <- paste0(
        input$selectStudy, input$selectContrast, input$selectEM
      )
      string
    })

#----------------------- COMPARISON

    output$upsetPlot <- renderPlot({
<<<<<<< Updated upstream
      get_random_ggplot(input$selectStudy)
    })

    output$upsetHtml <- renderUI({
      string <- paste0(
        input$selectStudy, input$selectContrast, input$upsetSelect
      )
      string
    })

=======
      req(input$selectStudy, input$upsetSelect)

      get_upset_plot(input$selectStudy, input$upsetSelect)
    })

>>>>>>> Stashed changes
  })
}
