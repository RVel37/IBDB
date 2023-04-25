
enrichr_path <- "enrichr_res.csv.gz"

en_df <- read_csv(enrichr_path)

#write table with enrichr results
DBI::dbWriteTable(conn, "enrichr", en_df)

gse_list <- c("GSE112057", "GSE123141", "GSE83687")


# EXPRESSION PLOT
get_exp_data <- function(gene_name, normalization) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite") #open conn

  df <- data.frame()

  #get correct table depending on normalization
  suffix <- switch(normalization,
                   CPM = "_cpm",
                   TPM = "_tpm",
                   RPKM = "_rpkm")

 for (gse in gse_list) {
    expTable <- paste0(gse, suffix) #loop through GSEs to find gene
    if (gene_name %in% dplyr::tbl(conn,expTable) %>%
        distinct(gene_id) %>% pull()) {

      df <- dplyr::bind_rows(df, dplyr::tbl(conn,expTable) %>%
        filter(gene_id == gene_name) %>%
        collect())
    }
 }

  enrichr <- dplyr::tbl(conn, "enrichr") %>%
    filter(Genes == gene_name) %>%
    select(Study_Contrast) %>%
    collect()

  dplyr::bind_rows(df, enrichr)

  return(df)
  DBI::dbDisconnect(conn) #close conn
}

