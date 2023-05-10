box::use(
  shiny[tabPanel,icon,tags,NS]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Documentation",
    id = "docs-tab",


    tags$iframe(
      src = 'www/documentation.html',
      width = '100%', height = '800px',
      frameborder = 0,
      scrolling = 'auto'
    ))
}
