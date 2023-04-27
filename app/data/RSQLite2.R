
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


#-------------------------------------------------
# EXPRESSION PLOT
#-------------------------------------------------

get_exp_data <- function(gene_name, normalization) {
conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite") #open conn

# use ens2sym to match inputted gene with ENSEMBL ID
ens2sym <- dplyr::tbl(conn, "ens2sym")
ens_id <- ens2sym %>%
  filter(gene_name == !!gene_name) %>%
  select(gene_id) %>%
  pull()

###### find correct study + normalization
#filter tables for normalization
suffix <- switch(normalization,CPM="_cpm",TPM="_tpm",RPKM="_rpkm")
gse_list <- c("GSE112057", "GSE123141", "GSE83687")

for (gse in gse_list) {
  expTable <- dplyr::tbl(conn, paste0(gse, suffix)) %>% collect() #loop through GSEs

  if (ens_id %in% expTable$gene_id) {
    expDf <- (expTable %>% filter (gene_id == ens_id))
  }
}

# add conditions (from metadata table)
metadata <- dplyr::tbl(conn,"metadata") %>% collect

longExpDf <- expDf %>%
  pivot_longer(cols = -gene_id, names_to="sample_id", values_to="expression") %>%
  collect()

longExpDf <- left_join(longExpDf,metadata, by = "sample_id")

return(expDf)
DBI::dbDisconnect(conn) #close conn
}


#------------- test variable
contrast <- "Crohn vs. Control"

#-------------------------------------------------
# VOLCANO PLOT
#-------------------------------------------------

get_volcano_data <- function(gene, contrast) {
conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite")

####### find correct study (GSE)
gse_list <- c("GSE112057", "GSE123141", "GSE83687")

degStudy <- dplyr::tbl(conn, paste0(study, "_degs")) %>% collect() # DEGs for selected study


######### Find selected gene (highlighted on volcano plot)
ens2sym <- dplyr::tbl(conn, "ens2sym") # extract ENSEMBL ID
ens_id <- ens2sym %>%
  filter(gene_name == !!gene_name) %>%
  select(gene_id) %>%
  pull()

selected_gene <- dplyr::tbl(conn, paste0(gse, "_degs")) %>%
  filter(gene_id == ens_id) %>%
  collect()


####### get contrasts
pair <- strsplit(contrast, " vs. ")[[1]]

contrasts <- degStudy %>%
  filter(numerator == pair[[1]] & denominator == pair[[2]])

###### combine into list (main data frame + highlighted gene)

vol_list <- list(contrasts, selected_gene)

return(vol_list)
DBI::dbDisconnect(conn)
}


