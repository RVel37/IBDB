# app/main.R

box::use(
  shiny[...],
  bslib[bs_theme],
  shinycssloaders[...],
  prompter[...],
  dplyr[...],
  tidyr[...],
  pheatmap[...],
  tibble[...],
  futile.logger[...],
  ggplot2[...],
  tidyverse[...]

)

box::use(
  # logic folder
  app / logic / data,
  app / logic / ui_globals,
  app / logic / utils,

  # view folder
  #app / view / ui,
  app / view / server,

  # data
  app / data / app_data.rds
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "IBDB",
      id = "IBDB",
      theme = bslib::bs_theme(bootswatch = "lumen"),
      tabPanel(
        title = "Home",
        id = "home-tab",
        value = "aboutTab",
        icon = icon("home"),
        fluidPage(br(), includeHTML("www/home.html"))
      ),
      tabPanel(
        title = "Explore",
        id = "explore-tab",
        icon = icon("table"),
        ExplorePageContents(deg_contrasts) # ExplorePageContents(results)
      ),
      tabPanel(
        title = "Download",
        id = "download-tab",
        icon = icon("download"),
        DownloadPageContents()
      ),
      tabPanel(
        title = "Documentation",
        id = "docs-tab",
        icon = icon("file-alt"),
        tags$iframe(
          src = "./documentation.html",
          width = "100%", height = "800px",
          frameborder = 0,
          scrolling = "auto"
        )
      )
    ),
    tags$footer(HTML(footerHTML()))
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    server$server("server")
  })
}
