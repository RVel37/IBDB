box::use(
  shiny[tabPanel,icon,tags,NS]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Documentation",
    id = "docs-tab",
    icon = icon('file-alt'),
    tags$iframe(
      src = './documentation.html',
      width = '100%', height = '800px',
      frameborder = 0,
      scrolling = 'auto'
    ))
}
