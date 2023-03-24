library(edgeR)
library(tidyverse)
library(DBI)
library(RSQLite)

#working directory: preprocess

# import data
metadata <- read.csv("metadata/metadatatemp.csv") #change to metadata
all_contrasts <- read.csv("metadata/contraststemp.csv")
  
# generate gene lengths
txdb <- GenomicFeatures::makeTxDbFromEnsembl(
  organism = "Homo sapiens",
  release = 107
)

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

rpkm_to_tpm <- function(x) {
  rpkm.sum <- colSums(x)
  return(t(t(x) / (1e-06 * rpkm.sum)))
}


#-----------------------------------
# for each unique study generate .csv.gx files for CPM, RPKM, TPM + DEGs
#-----------------------------------

unique_studies <- metadata %>%
  distinct(study_id) %>%
  pull()

# filter for the sample IDs relevant to current study
for (study_id in unique_studies) {
  exp_filename <- paste0(study_id, "_gene_exp.csv.gz")
  deg_filename <- paste0(study_id, "_degs.csv.gz")

  samples <- metadata %>%
    filter(study_id == !!study_id) %>%
    pull(sample_id)

  mat <- lapply(samples, function(id) {
    strand <- metadata %>%
      filter(sample_id == id) %>%
      pull(stranded)
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
  
  # Remove large unneeded R objects and free up some memory
  rm(mat)
  gc()
  
}




#---------------------------
#generate enrichr_res.csv.gz
#---------------------------

# Get study IDs
study_ids <- read_csv(metadata) %>% 
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
  filename <- paste0(study, deg_outfile_suffix)
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
  write_csv("enrichr_res.csv.gz")




#--------------------------
# generate final dataset to be converted into SQLite DB
#--------------------------



metadata_tbl <- read_csv(metadata)
contrasts_tbl <- read_csv(all_contrasts)
study_ids <- contrasts_tbl %>% 
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

# Get DEGs and expression levels, and add gene symbols with inner join (removes
# gene IDs without a symbol)
degs <- lapply(study_ids, function(id) {
  filename <- paste0(id, deg_outfile_suffix)
  read_csv(filename) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    relocate(gene_name) %>% 
    select(-gene_id)
})

exps <- lapply(study_ids, function(id) {
  filename <- paste0(id, exp_outfile_suffix)
  
  cpm <- read_csv(filename) %>% 
    select(gene_id, sample_id, cpm) %>% 
    pivot_wider(names_from = sample_id, values_from = cpm) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    relocate(gene_name) %>% 
    select(-gene_id)
  
  rpkm <- read_csv(filename) %>% 
    select(gene_id, sample_id, rpkm) %>% 
    pivot_wider(names_from = sample_id, values_from = rpkm) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    relocate(gene_name) %>% 
    select(-gene_id)
  
  tpm <- read_csv(filename) %>% 
    select(gene_id, sample_id, tpm) %>% 
    pivot_wider(names_from = sample_id, values_from = tpm) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    relocate(gene_name) %>% 
    select(-gene_id)
  
  list(cpm = cpm, rpkm = rpkm, tpm = tpm)
})

# Get enrichr results and convert to list
eres_tbl <- read_csv(ERES_CSV)

study_contrasts <- unique(eres_tbl$Study_Contrast)
names(study_contrasts) <- study_contrasts

eres <- lapply(as.list(study_contrasts), function(x) {
  eres_tbl %>% 
    filter(Study_Contrast == x) %>% 
    select(-Study_Contrast)
})

# Generate app data
app_data <-  list(
  metadata = metadata_tbl,
  contrasts = contrasts_tbl,
  degs = degs,
  exps = exps,
  eres = eres
)


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
