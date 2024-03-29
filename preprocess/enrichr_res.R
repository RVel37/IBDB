#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------
library(biomaRt)
library(enrichR)
library(tidyverse)
#-------------------------------------------------------------------------------
# CONSTANTS
#-------------------------------------------------------------------------------

OUTPUT_FILENAME <- "enrichr_res.csv.gz"

METADATA_CSV <- "processR/metadata.csv"
DEG_SUFFIX <- "_degs.csv.gz"

# Get study IDs
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
  filename <- paste0(study, DEG_SUFFIX)
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
    mutate(group = "Over-expressed",
           P.value = as.double(P.value),
           Adjusted.P.value = as.double(Adjusted.P.value),
           Old.P.value = as.integer(Old.P.value),
           Old.Adjusted.P.value = as.integer(Old.Adjusted.P.value),
           Odds.Ratio = as.double(Odds.Ratio),
           Combined.Score = as.character(Combined.Score),
           Genes = as.character(Genes),
           group = as.character(group)
    )%>%
    mutate_if(is.logical, as.character)

  resdn <- enrichr(dn_genes, databases = "KEGG_2019_Human") %>%
    pluck("KEGG_2019_Human") %>%
    mutate(group = "Under-expressed",
           P.value = as.double(P.value),
           Adjusted.P.value = as.double(Adjusted.P.value),
           Old.P.value = as.integer(Old.P.value),
           Old.Adjusted.P.value = as.integer(Old.Adjusted.P.value),
           Odds.Ratio = as.double(Odds.Ratio),
           Combined.Score = as.character(Combined.Score),
           Genes = as.character(Genes),
           group = as.character(group)
    )%>%
    mutate_if(is.logical, as.character)

  bind_rows(resup, resdn)
})

########## ERES MODIFIED ############
eres <- lapply(unique_groups, function(x) {
  up_genes <- degs %>%
    filter(group == x & logFC > 1) %>%
    pull(gene_name)
  dn_genes <- degs %>%
    filter(group == x & logFC < -1) %>%
    pull(gene_name)

  if (length(up_genes) > 0) {
    resup <- enrichr(up_genes, databases = "KEGG_2019_Human") %>%
      pluck("KEGG_2019_Human") %>%
      mutate(group = "Over-expressed",
             P.value = as.double(P.value),
             Adjusted.P.value = as.double(Adjusted.P.value),
             Old.P.value = as.integer(Old.P.value),
             Old.Adjusted.P.value = as.integer(Old.Adjusted.P.value),
             Odds.Ratio = as.double(Odds.Ratio),
             Combined.Score = as.character(Combined.Score),
             Genes = as.character(Genes),
             group = as.character(group)
      ) %>%
      mutate_if(is.logical, as.character)
  } else {
    resup <- NULL
  }

  if (length(dn_genes) > 0) {
    resdn <- enrichr(dn_genes, databases = "KEGG_2019_Human") %>%
      pluck("KEGG_2019_Human") %>%
      mutate(group = "Under-expressed",
             P.value = as.double(P.value),
             Adjusted.P.value = as.double(Adjusted.P.value),
             Old.P.value = as.integer(Old.P.value),
             Old.Adjusted.P.value = as.integer(Old.Adjusted.P.value),
             Odds.Ratio = as.double(Odds.Ratio),
             Combined.Score = as.character(Combined.Score),
             Genes = as.character(Genes),
             group = as.character(group)
      ) %>%
      mutate_if(is.logical, as.character)
  } else {
    resdn <- NULL
  }

  if (length(resup) > 0 & length(resdn) > 0) {
    bind_rows(resup, resdn)
  } else if (length(resup) > 0) {
    resup
  } else if (length(resdn) > 0) {
    resdn
  } else {
    NULL
  }
})

lapply(as.list(names(eres)), function(x) {
  eres[[x]] %>%
    mutate(Study_Contrast = x) %>%
    relocate(Study_Contrast)
}) %>%
  bind_rows() %>%
  write_csv(OUTPUT_FILENAME)
