box::use(
  shiny[...],
  DT[renderDT,DTOutput],
  dplyr[filter],
  shinycssloaders[withSpinner],
  shinipsum[...],
)
box::use(
  app/logic/b_explore_utils[get_random_ggplot],
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
# GSE drop-down
                selectInput(
                  inputId = ns("selectStudy"),
                  label = "Study",
                  selected = "line",
                  choices = c("line", "bin2d", "contour", "density")),

              ),  #here will go the uioutput (study link)

# study contrasts drop-down
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

# Results table
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
              id = "expTabset",
              tabPanel(
                title = "Expression",
                icon=icon('chart-bar'),
                selectInput(
                  inputId = ns("selectNorm"),
                  label = "Normalization",
                  selected = "CPM",
                  choices = c("CPM", "TPM", "RPKM")
                ),
                uiOutput(ns("html")),
                plotOutput(ns("expPlot"))
              ),
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

    output$volcano_plot <- renderPlot({
      random_ggplot()
      # create_exp_plot(input$selectStudy, input$selectContrast, input$selectNorm)
    })

    output$expPlot <- renderPlot({
      get_random_ggplot(input$selectStudy)
    })

    output$html <- renderUI({
      string <- paste0(
        input$selectStudy, input$selectContrast, input$selectNorm
      )
      string
    })

  })
}
