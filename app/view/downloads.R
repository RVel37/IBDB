box::use(
  DT[renderDT,datatable],dplyr[mutate]
)

box::use(
  app/logic/data[S3_HTTPS]
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Downloads
    output$downloadLinks <- DT::renderDT({
      tibble(
        File = c(
          
          #new data to go here:
          
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
        DT::datatable(
          selection = list(mode = "none"),
          rownames = FALSE, escape = FALSE, options = list(dom = "t")
        )
    })
  })
}
