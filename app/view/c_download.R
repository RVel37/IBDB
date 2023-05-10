box::use(
  shiny[tabPanel,icon,NS],
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

  DownloadPageContents()
  )
}

#--------------------------------
# SERVER
#--------------------------------

#'@export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

output$downloadLinks <- DT$renderDT({
  tibble(
    File = c(
      "metadata.csv",
      "contrasts.csv",
      "GSE126848_degs.csv.gz",
      "GSE135251_degs.csv.gz",
      "GSE126848_gene_exp.csv.gz",
      "GSE135251_gene_exp.csv.gz",
      "enrichr_res.csv.gz"
    )
  ) %>%
    mutate(
      Download = paste0(
        "<a href='",
        paste0(S3_HTTPS, File),
        "' target='_blank'>link</a>"
      )
    ) %>%
    DT$datatable(
      selection = list(mode = "none"),
      rownames = FALSE, escape = FALSE, options = list(dom = 't')
      )
    })

  })
}
