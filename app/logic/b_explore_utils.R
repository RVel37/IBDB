box::use(
  shiny[fluidPage,fluidRow,column]
)



ExplorePageContents <- function(deg_contrasts) {
  fluidPage(
    title = "Explore",
    fluidRow(
      column(
        width = 4,
        GeneTable_panel(deg_contrasts)
      ),
      column(
        width = 8,
        OutputPanel_tabset()
      )
    )
  )
}

