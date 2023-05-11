box::use(
  shiny[tabPanel,icon,tags,NS,includeHTML]
)

#'@export
ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Documentation",
    id = "docs-tab"

    #includeHTML("www/documentation.html")
    )
}
