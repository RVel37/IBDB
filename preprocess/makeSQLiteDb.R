library(edgeR)
library(tidyverse)
library(DBI)
library(enrichR)
library(RMariaDB)
library(biomaRt)
library(RSQLite)

# import data
metadata <- read.csv("preprocess/metadata/metadata.csv") 
all_contrasts <- read.csv("preprocess/metadata/contrasts.csv")
counts_dir <- "preprocess/raw_counts"
gtf <- "preprocess/refs/Homo_sapiens.GRCh38.107.gtf.gz"


# generate gene lengths
txdb <- GenomicFeatures::makeTxDbFromGFF(file = gtf)
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

#-------------------------------------------------------------------------------
# Process Raw Counts - generates 3 exp.csv.gz files
#-------------------------------------------------------------------------------

# 3 GSEs
unique_studies <- metadata %>% distinct(study_id) %>% pull()



for (study_id in unique_studies) {
exp_filename <- paste0(study_id, exp_outfile_suffix)
deg_filename <- paste0(study_id, deg_outfile_suffix)

# Filter sample IDs list relevant to current study
samples <- metadata %>% filter(study_id == !!study_id) %>% pull(sample_id)

# Build raw count matrix
mat <- lapply(samples, function(id) {
  strand <- metadata %>% filter(sample_id == id) %>% pull(strandedness)
  count_path <- file.path(counts_dir, paste0(id, "_ReadsPerGene.out.tab"))
  
  read_tsv(count_path,
           skip = 4, 
           col_names = c("gene_id", "unstranded", "forward", "reverse")
  ) %>%
    select(gene_id, contains(!!strand)) %>%
    rename(counts = contains(!!strand)) %>%
    mutate(sample_id = id)
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = sample_id, values_from = counts) %>%
  column_to_rownames("gene_id") %>%
  as.matrix() %>% na.omit() #works after na.omit which removes some ENS IDs


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



# DEG files ------------------------------------------------------------


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
    AnnotationDbi::select(gene_id, numerator, denominator, logFC, FDR)
}) %>% 
  bind_rows() %>% 
  write_csv(deg_filename)

# Remove large unneeded R objects and free up some memory
rm(mat)
gc()


}






#-------------------------------------------------------------------------------
# enrichr res
#-------------------------------------------------------------------------------

study_ids <- metadata %>% 
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
  filename <- file.path(subdir,paste0(study, deg_outfile_suffix))
  read_csv(filename) %>% 
    inner_join(ens2sym, by = c("gene_id")) %>% 
    mutate(study_id = study)
}) %>% 
  bind_rows() %>% 
  filter(!is.na(FDR) & FDR < .01 & abs(logFC) > 1) %>%
  unite("group", c("study_id", "numerator", "denominator"))




