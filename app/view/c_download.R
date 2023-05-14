box::use(
  shiny[...],
  DT,
  dplyr[...],
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
    # DownloadPageContents(),
    includeMarkdown("app/static/downloads.md"),
  )
}

#--------------------------------
# SERVER
#--------------------------------

#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
