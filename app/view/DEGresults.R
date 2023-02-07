box::use(
  shiny[moduleServer],
  dplyr[filter,select,mutate,pull],
  DT[datatable,renderDT,formatSignif]
)

box::use(
  app/logic/data[degs],
  app/view/memory[link_server]
)

#'@export
server <- function(id) {
  moduleServer(
    id, 
    
    function(input, output, session) {
      
      # DEG results table
      output$degTable <- DT::renderDT(server = TRUE, {
        req(input$selectStudy, input$selectContrast)
        study <- input$selectStudy
        pair <- strsplit(input$selectContrast, " vs. ")[[1]]
        
        degs[[study]] %>%
          dplyr::filter(numerator == pair[[1]] & denominator == pair[[2]]) %>% 
          select(gene_name, logFC, FDR) %>% 
          mutate(
            gene_name = paste0(
              "<a href='", paste0(GENECARDS_BASE, gene_name),
              "' target='_blank'>", gene_name, "</a>"
            )
          ) %>%
          DT::datatable(
            selection = list(mode = "single", selected = 1),
            rownames = FALSE, escape = FALSE,
            colnames = c("Gene", "Fold Change (log2)", "Adjusted p-value"),
            options = list(pageLength = 8, scrollX = TRUE)
          ) %>% 
          DT::formatSignif(2:3, digits = 5)
      })
      
      # Get currently-selected gene from DEG results table
      current_gene <- reactive({
        req(input$selectStudy, input$selectContrast)
        study <- input$selectStudy
        pair <- strsplit(input$selectContrast, " vs. ")[[1]]
        
        # Get selected row from datatable
        selectedRow <- ifelse(
          is.null(input$degTable_rows_selected), 1, input$degTable_rows_selected
        )
        
        gene <- degs[[study]] %>%
          dplyr::filter(numerator == pair[[1]] & denominator == pair[[2]]) %>% 
          dplyr::filter(row_number() == selectedRow) %>%
          pull(gene_name)
      })
        
  })   
}
