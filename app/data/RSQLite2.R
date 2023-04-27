
enrichr_path <- "enrichr_res.csv.gz"
en_df <- read_csv(enrichr_path)

contrasts_path <-"contrasts.csv"
cont_df <- read_csv(contrasts_path)
metadata_path <-"metadata.csv"
met_df <- read_csv(metadata_path)

#write tables
DBI::dbWriteTable(conn, "enrichr", en_df)
DBI::dbWriteTable(conn, "contrasts",cont_df)
DBI::dbWriteTable(conn, "metadata", met_df)

gse_list <- c("GSE112057", "GSE123141", "GSE83687")

#----------- test variables (to be deleted)
normalization <- "CPM"
gene_name <- "MT-RNR1"

#-------------------------------------------------
get_exp_plot_data <- function(gene_name, normalization) {
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
suffix <- switch(normalization,
                 CPM = "_cpm",
                 TPM = "_tpm",
                 RPKM = "_rpkm")

for (gse in gse_list) {
  expTable <- dplyr::tbl(conn, paste0(gse, suffix)) %>% collect() #loop through GSEs

  if (ens_id %in% expTable$gene_id) {
    expDf <- (expTable %>% filter (gene_id == ens_id))
  }
}

# add conditions (from metadata table)
metadata <- dplyr::tbl(conn,"metadata") %>% collect

longExpDf <- expDf %>% pivot_longer(cols = -gene_id, names_to="sample_id", values_to="expression") %>%
  collect()

longExpDf <- left_join(longExpDf,metadata, by = "sample_id")

return(expDf)
DBI::dbDisconnect(conn)
}

