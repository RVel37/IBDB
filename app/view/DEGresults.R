box::use(
  shiny[moduleServer,reactive,selectInput,renderUI],
  dplyr[filter,select,mutate,pull],
  DT[datatable,renderDT,formatSignif]
)

box::use(
  app/logic/data[degs126,degs135,deg_contrasts],
  app/view/memory[link_server]
)

#'@export
server <- function(id) {
  moduleServer(
    id, 
    
    function(input, output, session) {
      
      GEO_BASE <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="
      GENECARDS_BASE <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
      S3_HTTPS <- "https://liverdb-data.s3.amazonaws.com/"
      
      app_data <- readRDS("app_data.rds")
      
      exps <- app_data[["exps"]]
      exps126 <- exps[["GSE126848"]]
      exps135 <- exps[["GSE135251"]] 
      
      degs <- app_data[["degs"]]
      degs126 <- degs[["GSE126848"]]
      degs135 <- degs[["GSE135251"]]
      
      
      # reactive contrast selector
      output$UIselectContrast <- renderUI({
        study <- input$selectStudy
        cont_labels <- deg_contrasts %>%
          dplyr::filter(study_id == study) %>% 
          unite("contrast", c("numerator", "denominator"), sep = " vs. ") %>% 
          pull(contrast)
        
        selectInput(
          inputId = "selectContrast",
          label = "Contrast (Numerator vs. Denominator)",
          selected = cont_labels[1],
          choices = cont_labels)
      })
      
      # DEG results table
      output$degTable <- DT::renderDT(server = TRUE, {
        req(input$selectStudy, input$selectContrast)
        study <- input$selectStudy
        pair <- strsplit(input$selectContrast, " vs. ")[[1]]
      
      
      degs126filtered <- degs126 %>%
        dplyr::filter(numerator == pair[[1]]) %>% 
        select(gene_name, logFC, FDR)
      
      degs135filtered <- degs135 %>%
        dplyr::filter(denominator == pair[[2]]) %>% 
        select(gene_name, logFC, FDR)
      
      mergedDegs <- dplyr::full_join(degs126filtered, 
                                     degs135filtered, by = "gene_name")
      
      mergedDegs %>% 
        mutate(
          gene_name = paste0(
            "<a href='", paste0(GENECARDS_BASE, gene_name),
            "' target='_blank'>", gene_name, "</a>"
          )
        ) %>%
        DT::datatable(
          selection = list(mode = "single", selected = 1),
          rownames = FALSE, escape = FALSE,
          colnames = c("Gene", "Fold Change (log2)", 
                       "Adjusted p-value", "Fold Change (log2)", 
                       "Adjusted p-value"),
          options = list(pageLength = 8, scrollX = TRUE)
        ) %>% 
        DT::formatSignif(c(3,5), digits = 5)
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
        
        if(study == "GSE126848"){
          gene <- degs126 %>%
            dplyr::filter(numerator == pair[[1]] & denominator == pair[[2]])%>%
            dplyr::filter(row_number() == selectedRow) %>%
            pull(gene_name)
        } else if (study == "GSE135251"){
          gene <- degs135 %>%
            dplyr::filter(numerator == pair[[1]] & denominator == pair[[2]])%>% 
            dplyr::filter(row_number() == selectedRow) %>%
            pull(gene_name)
        }
      })
  })   
}
