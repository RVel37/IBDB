box::use(
  shiny[tabPanel,icon,NS]
)

box::use(
  app/logic/c_download_utils[DownloadPageContents]
)

#'@export
ui <- function(id) {
  ns <- NS(id)

tabPanel(
  title = "Download",
  id = "download-tab",
  icon = icon('download'),
  DownloadPageContents()
  )
}