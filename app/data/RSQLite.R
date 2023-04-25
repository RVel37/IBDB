library(tidyverse)

setwd("app/data")

deg_paths <-  c(
  "GSE112057_degs.csv.gz",
  "GSE123141_degs.csv.gz",
  "GSE83687_degs.csv.gz"
)
exp_paths <- c(
  "GSE112057_gene_exp.csv.gz",
  "GSE123141_gene_exp.csv.gz",
  "GSE83687_gene_exp.csv.gz"
)

# --- DATABASE CREATION --------------------------------------------------------
# Creates an empty sqlite database in your working directory if none exists
# Other it connects to an existing db if it exists
# `conn` is your connection to the db
conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite")


# Add DEGs to the sqlite db
for (path in deg_paths) {
  # Strip ".csv.gz" from the path
  table_name <- str_replace(path, ".csv.gz", "")

  # Load the .csv.gz given in `path`
  df <- read_csv(path)

  # Add the dataframe to the db
  DBI::dbWriteTable(conn, table_name, df)
}


# Add gene expressions to the sqlite db
for (path in exp_paths) {
  # Get the study id
  study_id <- str_replace(path, "_gene_exp.csv.gz", "")

  # Load the .csv.gz given in `path`
  df <- read_csv(path)

  # Convert each expression type from long to wide
  # Wide format saves a lot of disk space
  cpm <- df %>%
    select(gene_id, sample_id, cpm) %>%
    pivot_wider(names_from = "sample_id", values_from = "cpm")
  rpkm <- df %>%
    select(gene_id, sample_id, rpkm) %>%
    pivot_wider(names_from = "sample_id", values_from = "rpkm")
  tpm <- df %>%
    select(gene_id, sample_id, tpm) %>%
    pivot_wider(names_from = "sample_id", values_from = "tpm")

  # Add each dataframe the db
  table_name <- paste0(study_id, "_cpm")
  DBI::dbWriteTable(conn, table_name, cpm)
  table_name <- paste0(study_id, "_rpkm")
  DBI::dbWriteTable(conn, table_name, rpkm)
  table_name <- paste0(study_id, "_tpm")
  DBI::dbWriteTable(conn, table_name, tpm)

  # Rm large local variables
  rm(df, cpm, rpkm, tpm)
}


# Get Ensembl ID to HGNC symbol mapping
ens2sym <- biomaRt::getBM(
  attributes = c("ensembl_gene_id","hgnc_symbol"),
  mart = biomaRt::useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")
) %>%
  filter(hgnc_symbol != "") %>%
  distinct(ensembl_gene_id, .keep_all = TRUE) %>%
  rename(gene_id = ensembl_gene_id, gene_name = hgnc_symbol)

# Add ens2sym to the db
DBI::dbWriteTable(conn, "ens2sym", ens2sym)


# You should now have a .sqlite file in your working directory


# --- DATABASE EXPLORATION -----------------------------------------------------
# Run these line by line to inspect what's going on

# List out all the tables available in the db
DBI::dbListTables(conn)

# List the column names of a given table in the db
DBI::dbListFields(conn, "GSE123141_degs")

# Get a connection to a given table
tbl_conn <- dplyr::tbl(conn, "GSE123141_degs")

# If you have dplyr loaded (we do here bc of tidyverse), you can use dplyr
# functions with tbl_conn
tbl_conn %>% head()

# Notice that it is NOT a dataframe
class(tbl_conn %>% head())

# This means trying to access a column directly from tbl_conn won't work
tbl_conn$gene_id

# It is only a dataframe when we finish with collect()
class(tbl_conn %>% head() %>% collect())

# You can do your usual dplyr things, just remember to collect() if you need a df
tbl_conn %>%
  filter(logFC > 7) %>%
  filter(FDR < 1e-8) %>%
  collect()

# The point of collect() is that the data isn't loaded into memory until you
# call collect(). Consider two versions of code the give the same output:

# This will first load ALL rows from disk (.sqlite) to memory (R variable), THEN
# truncate the dataframe
tbl_conn %>% collect() %>% head()

# This will truncate the table AT THE DATABASE, then ONLY load the first few rows
# into R
tbl_conn %>% head() %>% collect()

# Here is an example to show you that you can do complex queries with a db
dplyr::tbl(conn, "GSE83687_degs") %>%  # Start with a DEGs table
  filter(numerator == "Crohn") %>%  # Filter for Crohn's DEGs
  filter(logFC > 3.8) %>%  # Filter for high logFC
  left_join(dplyr::tbl(conn, "ens2sym"), by = "gene_id") %>%  # Left join with table with gene names
  mutate(gene_name = case_when(
    is.na(gene_name) ~ gene_id,  # Replace NAs in the gene_name with gene_id
    TRUE ~ gene_name
  )) %>%
  collect()

# collect() doesn't strictly need to be at the end

# Close the connection to the db when you're done
DBI::dbDisconnect(conn)


# --- NEXT STEPS ---------------------------------------------------------------
# 1) Create a new table in this db with the enrichr results
# 2) For each plot, write functions to extract relevant data from the db
#    Eg. for the expression plot you want something like:
#
#    get_exp_plot_data <- function(gene_name, normalization) {
#      conn <- DBI$dbConnect(...)
#
#      ...  # Code that extracts the correct info
#
#      DBI$dbConnect(conn)
#
#      df  # Return a dataframe with the queried data
#    }
