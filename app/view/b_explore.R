box::use(
  dplyr[filter, pull, row_number],
  DT,
  plotly,
  shiny[...],
  shinycssloaders[withSpinner],
  shinipsum[...],
)
box::use(
  app/logic/b_explore_utils[
    get_exp_plotly, get_enrich_plot, get_upset_plot, get_volcano, get_heatmap,
      ],
  app/logic/db_utils,
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
# GSE drop-down -----------------------------
                width = 6,
                selectInput(
                  inputId = ns("selectStudy"),
                  label = "Study",
                  selected = "GSE83687",
                  choices = c("GSE83687", "GSE112057", "GSE123141")
                ),
              ),  #here will go the uioutput (study link)

# study contrasts drop-down ------------------
              column(
                width = 6,
                uiOutput(ns("UIselectContrast"))
              )
            ),
            hr(),

            fluidRow(
              column(
                width = 12,

# Results table ----------------------------
                h3("Results Table"),
                p("IBD RNA-Seq DEG analysis results."),
                withSpinner(DT$DTOutput(ns("degTable")))
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
                db_utils$makeHeaders(
                  title = "Gene counts ",
                  message=paste0("Gene count plots for samples in the selected study.")
                ),
                hr(),
                plotly$plotlyOutput(ns("expPlot"), height = "500px")
              ),
# Volcano plot----------------------------
              tabPanel(
                title = "Volcano plot",
                icon=icon('mountain'),
                fluidRow(
                  column(
                    width = 6,
                    hr(),
                    db_utils$makeHeaders(
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
                    db_utils$makeHeaders(
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
                    db_utils$makeHeaders(
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
                      inputId = ns("selectEM"),
                      choices = c("Combined.Score", "Odds.Ratio", "Padj (-log10)"),
                      selected = "Combined.Score",
                      label = "Enrichment Metric"
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotOutput(ns("enrichPlot"), height = "500px")
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
                    db_utils$makeHeaders(
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
                      inputId = ns("upsetSelect"),
                      choices = c("Over-expressed", "Under-expressed"),
                      selected = "Over-expressed",
                      label = "DEG type"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotOutput(ns("upsetPlot"), height = "500px")
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
    ns <- session$ns

    # Reactive contrast selector
    output$UIselectContrast <- renderUI({
      study <- input$selectStudy

      contrasts <- db_utils$get_contrasts(study)

      selectInput(
        inputId = ns("selectContrast"),
        label = "Contrast (Numerator vs. Denominator)",
        selected = contrasts[1],
        choices = contrasts
      )
    })

    # Load DEGs dataframe
    degs_df <- reactive({
      db_utils$get_deg_df(input$selectStudy, input$selectContrast)
    })

    # DEG results table
    output$degTable <- DT$renderDT(server = TRUE,{
      req(input$selectStudy, input$selectContrast)

      degs_df() %>%
        DT$datatable(
          selection = list(mode = "single", selected = 1),
          rownames = FALSE, escape = FALSE,
          colnames = c("Gene", "Fold Change (log2)", "Adjusted p-value"),
          options = list(pageLength = 8, scrollX = TRUE)
        ) %>%
        DT$formatSignif(2:3, digits = 5)
    })

    # Row selector: finds the gene name from the row selected in the DEGs DT
    current_gene <- reactive({
      req(input$degTable_rows_selected)

      selected_row <- ifelse(
        is.null(input$degTable_rows_selected), 1, input$degTable_rows_selected
      )

      gene <- degs_df() %>%
        filter(row_number() == selected_row) %>%
        pull(gene_name)

      return(gene)
    })


#-------------------------------------------------------
# plots for each tab
#-------------------------------------------------------

#----------------------- EXPRESSION PLOT

    output$expPlot <- plotly$renderPlotly({
      req(input$selectStudy, input$selectNormExp, current_gene())

      get_exp_plotly(input$selectStudy, input$selectNormExp, current_gene())
    })

#----------------------- VOLCANO PLOT

    output$volcanoPlot <- renderPlot({
      req(input$selectStudy, input$selectContrast, current_gene())

      get_volcano(input$selectStudy, input$selectContrast, current_gene())
    })

#----------------------- HEATMAP

    output$heatmap <- renderPlot({
      req(input$selectStudy, input$selectContrast, input$selectNormHeatmap)

      get_heatmap(input$selectStudy, input$selectContrast, input$selectNormHeatmap)
    })

#----------------------- PATHWAY ANALYSIS

    output$enrichPlot <- renderPlot({
      req(input$selectStudy, input$selectContrast, input$selectEM)

      get_enrich_plot(input$selectStudy, input$selectContrast, input$selectEM)
    })


#----------------------- COMPARISON

    output$upsetPlot <- renderPlot({
      req(input$selectStudy, input$upsetSelect)

      get_upset_plot(input$selectStudy, input$upsetSelect)
    })

  })
}
