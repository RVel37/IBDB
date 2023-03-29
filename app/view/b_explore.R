box::use(
  shiny[NS,tabPanel,icon]
)

box::use(
  app/logic/b_explore_utils[ExplorePageContents]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Explore",
    id = "explore-tab",
    icon = icon('table'),
    ExplorePageContents(deg_contrasts) #ExplorePageContents(results)
  )
}

