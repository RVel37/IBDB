
enrichr_path <- "enrichr_res.csv.gz"

en_df <- read_csv(enrichr_path)

#write table with enrichr results
DBI::dbWriteTable(conn, "enrichr", en_df)

gse_list <- c("GSE112057", "GSE123141", "GSE83687")

CPM <- "CPM"
gene_name <- "MT-RNR1"

#-------------------------------------------------

# EXPRESSION PLOT
conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite") #open conn

# use ens2sym to match inputted gene with ENSEMBL ID
ens2sym <- dplyr::tbl(conn, "ens2sym")

ens_id <- ens2sym %>%
  filter(gene_name == !!gene_name) %>%
  select(gene_id) %>%
  pull()

###### find correct GSE + correct normalization

#filter tables for normalization
suffix <- switch(CPM, #normalization,
                 CPM = "_cpm",
                 TPM = "_tpm",
                 RPKM = "_rpkm")

for (gse in gse_list) {
  expTable <- dplyr::tbl(conn, paste0(gse, suffix)) %>% collect() #loop through GSEs

  if (ens_id %in% expTable$gene_id) {
    expDf <- (expTable %>% filter (gene_id == ens_id))
  }
}

###### find gene in in enrichr
enrichr <- tbl(conn,"enrichr") %>% collect()

contrasts <- enrichr %>%
  filter(Genes == gene_name) %>%
  pull(Study_Contrast) #--------------------- needs work

df <-


return(expDf)
DBI::dbDisconnect(conn)















