box::use(
  shiny[tabPanel,fluidPage,NS,icon,br,includeHTML],
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
  title = "Home",
  id = "home-tab",
  value = "aboutTab",
  icon = icon("home"),
  fluidPage(br(), includeHTML("www/home.html"))
  )
}

