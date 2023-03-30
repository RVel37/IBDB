#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------
library(edgeR)
library(tidyverse)
library(biomaRt)
library(enrichR)

#-------------------------------------------------------------------------------
# CONSTANTS
#-------------------------------------------------------------------------------

COUNTS_DIR <- "preprocess/raw_counts"
COUNTS_SUFFIX <- "_ReadsPerGene.out.tab"
COUNTS_SKIP_LINES <- 4

GTF_FILE <- "preprocess/refs/Homo_sapiens.GRCh38.107.gtf.gz"

METADATA_CSV <- "preprocess/metadata/metadata.csv"
CONTRASTS_CSV <- "preprocess/metadata/contrasts.csv"

EXP_OUTFILE_SUFFIX <- "_gene_exp.csv.gz"
DEG_OUTFILE_SUFFIX <- "_degs.csv.gz"


#-------------------------------------------------------------------------------
# HELPER FUNCTIONS
#-------------------------------------------------------------------------------

mat_longify <- function(x) {
  as.data.frame(x) %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(cols = contains("SRR"))
}

# http://luisvalesilva.com/datasimple/rna-seq_units.html
rpkm_to_tpm <- function(x){
  rpkm.sum <- colSums(x)
  return(t(t(x) / (1e-06 * rpkm.sum)))
}


#-------------------------------------------------------------------------------
# process raw counts
#-------------------------------------------------------------------------------


metadata <- read_csv(METADATA_CSV)
all_contrasts <- read_csv(CONTRASTS_CSV)

# Get gene lengths
txdb <- GenomicFeatures::makeTxDbFromGFF(file = GTF_FILE)
gene_lengths <- GenomicFeatures::transcriptsBy(txdb, "gene") %>%
  GenomicRanges::reduce() %>%
  GenomicRanges::width() %>%
  sum()

# For each unique study, generate .csv.gz files for CPM/RKPM/TPM and DEGs
unique_studies <- metadata %>% distinct(study_id) %>% pull()
for (study_id in unique_studies) {
  exp_filename <- paste0(study_id, EXP_OUTFILE_SUFFIX)
  deg_filename <- paste0(study_id, DEG_OUTFILE_SUFFIX)
  
  # Filter sample IDs list relevant to current study
  samples <- metadata %>% filter(study_id == !!study_id) %>% pull(sample_id)
  
  # Build raw count matrix
  mat <- lapply(samples, function(id) {
    strand <- metadata %>% filter(sample_id == id) %>% pull(strandedness)
    count_path <- file.path(COUNTS_DIR, paste0(id, COUNTS_SUFFIX))
    
    read_tsv(count_path,
             skip = COUNTS_SKIP_LINES, 
             col_names = c("gene_id", "unstranded", "forward", "reverse")
    ) %>%
      select(gene_id, contains(!!strand)) %>%
      rename(counts = contains(!!strand)) %>%
      mutate(sample_id = id)
  }) %>%
    bind_rows() %>%
    pivot_wider(names_from = sample_id, values_from = counts) %>%
    column_to_rownames("gene_id") %>%
    as.matrix() %>% na.omit()
  
  # Compute CPM, RPKM, TPM
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
  
  # Write CPM, RPKM and TPM to .csv.gz
  cpms %>% 
    inner_join(rpkms, by = c("gene_id", "sample_id")) %>% 
    inner_join(tpms, by = c("gene_id", "sample_id")) %>% 
    write_csv(exp_filename)
  
  subdir <- paste0("preprocess/refs")
  file.rename(exp_filename,file.path(subdir, exp_filename)) #send to new dir
  
  
  # Remove large unneeded R objects and free up some memory
  rm(cpms, rpkms, rpkms_mat, tpms)
  gc()
  
  
  # Get contrasts relevant to current study
  study_contrasts <- all_contrasts %>%
    filter(study_id == !!study_id)
  
  # Get DEGs for each contrast and write to .csv.gz
  apply(study_contrasts, 1, function(x) {
    # Define current contrast
    numerator <-  x[["numerator"]]
    denominator <- x[["denominator"]]
    
    # Get relevant samples according to current contrast
    deg_meta <- metadata %>%
      filter(study_id == !!study_id) %>%
      mutate(group = case_when(
        str_starts(condition, numerator) ~ 1,
        str_starts(condition, denominator) ~ 2,
        TRUE ~ 0
      )) %>%
      filter(group != 0)
    
    groups <- factor(deg_meta$group)
    design <- model.matrix(~0 + groups)
    
    # DGE analysis
    mat[, deg_meta$sample_id] %>%
      DGEList(group = groups) %>%  
      calcNormFactors() %>%
      estimateGLMCommonDisp(design) %>%
      estimateGLMTrendedDisp(design) %>%
      estimateGLMTagwiseDisp(design) %>%
      glmFit(design) %>%
      glmLRT(contrast = c(1, -1)) %>%
      topTags(n="Inf") %>% 
      pluck("table") %>%
      mutate(
        numerator = numerator,
        denominator = denominator
      ) %>% 
      rownames_to_column("gene_id") %>% 
      select(gene_id, numerator, denominator, logFC, FDR)
  }) %>% 
    bind_rows() %>% 
    write_csv(deg_filename)
  
  subdir <- paste0("preprocess/refs")
  file.rename(deg_filename,file.path(subdir, deg_filename)) #send to new dir
  
  # Remove large unneeded R objects and free up some memory
  rm(mat)
  gc()
  
}



#-------------------------------------------------------------------------------
# enrichr res
#-------------------------------------------------------------------------------

enrichr_filename <- "enrichr_res.csv.gz"
deg_dir <- "preprocess/refs"

study_ids <- read_csv(METADATA_CSV) %>% 
  pull(study_id) %>% 
  unique()
names(study_ids) <- study_ids


# Get gene symbols from BioMart
ens2sym <- getBM(
  attributes = c("ensembl_gene_id","hgnc_symbol"),
  mart = useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")
) %>% 
  filter(hgnc_symbol != "") %>%
  rename(gene_id = ensembl_gene_id, gene_name = hgnc_symbol)

# Get DEG tables with gene symbols, and filter and label sig DEGs
degs <- lapply(study_ids, function(study){
  filename <- file.path(deg_dir, paste0(study, DEG_OUTFILE_SUFFIX))
  read_csv(filename) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    mutate(study_id = study)
}) %>% 
  bind_rows() %>% 
  filter(!is.na(FDR) & FDR < .01 & abs(logFC) > 1) %>%
  unite("group", c("study_id", "numerator", "denominator"))

# Get unique groups of enrichr runs
unique_groups <- degs %>%
  pull(group) %>%
  unique()
names(unique_groups) <- unique_groups

# Send genes to enrichr
eres <- lapply(unique_groups, function(x) {
  up_genes <- degs %>% 
    filter(group == x & logFC > 1) %>% 
    pull(gene_name)
  dn_genes <- degs %>% 
    filter(group == x & logFC < -1) %>% 
    pull(gene_name)
  
  resup <- enrichr(up_genes, databases = "KEGG_2019_Human") %>%
    pluck("KEGG_2019_Human") %>%
    mutate(group = "Over-expressed")
  resdn <- enrichr(dn_genes, databases = "KEGG_2019_Human") %>%
    pluck("KEGG_2019_Human") %>%
    mutate(group = "Under-expressed")
  
  bind_rows(resup, resdn)
})

lapply(as.list(names(eres)), function(x) {
  eres[[x]] %>% 
    mutate(Study_Contrast = x) %>% 
    relocate(Study_Contrast)
}) %>% 
  bind_rows() %>% 
  write_csv(enrichr_filename)
  
subdir <- paste0("preprocess/refs")
file.rename(enrichr_filename,file.path(subdir, enrichr_filename)) 





#----------------------------
# RSQLite (on-disk DB)
#----------------------------

# store permanently on disk
conn <- dbConnect(RSQLite::SQLite(), "appdata.sqlite")

# write data to table
RSQLite::dbWriteTable(conn, "App_data", app_data)

for (name in names(app_data)){
  table_name <- paste0("tbl_", name)
  dbWriteTable(conn, table_name, app_data[[name]])
}

dbListTables(conn) # check what tables exist in db
dbReadTable(conn, "App_data") # see table contents
dbRollback(conn) # restore previous version of a db

