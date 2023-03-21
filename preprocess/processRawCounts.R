 
library(edgeR)
library(tidyverse)
library(DBI)
library(RSQLite)

#import data
metadata <- read.csv("metadata/metadata.csv")
all_contrasts <- read.csv("contrasts.csv")

#generate gene lengths
txdb <- GenomicFeatures::makeTxDbFromEnsembl(
                        organism="Homo sapiens",
                        release=107)

gene_lengths <- GenomicFeatures::transcriptsBy(txdb, "gene") %>%
  GenomicRanges::reduce() %>%
  GenomicRanges::width() %>%
  sum()

exp_outfile_suffix <- "_gene_exp.csv.gz"
deg_outfile_suffix <- "_degs.csv.gz"


# convert matrix into DF
mat_longify <- function(x) {
  as.data.frame(x) %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(cols = contains("SRR"))
}

rpkm_to_tpm <- function(x){
  rpkm.sum <- colSums(x)
  return(t(t(x) / (1e-06 * rpkm.sum)))
}

#----------------------------
#generate .csv.gx files for CPM, RPKM, TPM + DEGs

unique_studies <- metadata %>% distinct(study_id) %>% pull()
for (study_id in unique_studies) {
  exp_filename <- paste0(study_id, "_gene_exp.csv.gz")
  deg_filename <- paste0(study_id, "_degs.csv.gz")
  
  samples <- metadata %>% filter(study_id == !!study_id) %>% pull(sample_id)
  
  mat <- lapply(samples, function(id) {
    strand <- metadata %>% filter(sample_id == id) %>% pull(strandedness)
    count_path <- file.path("raw_counts", paste0(id, "_ReadsPerGene.out.tab"))
    
    read_tsv(count_path,
             skip = 4, 
             col_names = c("gene_id", "unstranded", "reverse")
    ) %>%
      select(gene_id, contains(!!strand)) %>%
      rename(counts = contains(!!strand)) %>%
      mutate(sample_id = id)
  }) %>%
    bind_rows() %>%
    pivot_wider(names_from = sample_id, values_from = counts) %>%
    column_to_rownames("gene_id") %>%
    as.matrix()
  
  cpms <- mat %>%
    DGEList() %>%
    calcNormFactors() %>%
    cpm() %>% 
    mat_longify() %>%
    rename(sample_id = name, cpm = value)
  
  rpkms_mat <- rpkm(mat, gene_lengths[rownames(mat)])
  rpkms <- rpkms_mat %>%
    mat_longify() %>%
    rename(sample_id = name, rpkm = value)
  
  tpms <- rpkm_to_tpm(rpkms_mat) %>%
    mat_longify() %>%
    rename(sample_id = name, tpm = value)
  
  cpms %>% 
    inner_join(rpkms, by = c("gene_id", "sample_id")) %>% 
    inner_join(tpms, by = c("gene_id", "sample_id")) %>% 
    write_csv(paste0(study_id, "_gene_exp.csv.gz"))
  
  rm(cpms, rpkms, rpkms_mat, tpms)
  gc()
  
}


#work in progress

#-------------------------
#RSQLite

#store permanently on disk
conn <- dbConnect(RSQLite::SQLite(), "appdata.sqlite")

#write data to table
RSQLite::dbWriteTable(conn, "App_data", app_data)

dbListTables(conn) #check what tables exist in db
dbReadTable(conn, "App_data") #see table contents
dbRollback(conn) #restore previous version of a db

