box::use(
  shiny[tagList,tags,tabPanel,fluidPage,
        NS,icon,br,includeHTML,HTML],
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
  title = "Home",
  id = "home-tab",
  value = "aboutTab",
  
  fluidPage(br(), includeHTML("www/home.html"))
  )
}
