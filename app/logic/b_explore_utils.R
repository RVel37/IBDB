box::use(
  ComplexHeatmap,
  DBI,
  dplyr[...],
  ggplot2[...],
  grid,
  plotly,
  prompter[add_prompt],
  shiny[...],
  tibble[column_to_rownames],
  tidyr[pivot_longer, pivot_wider],
  )
box::use(
  app/logic/db_utils,
)


# --- EXPRESSION PLOT ----------------------------------------------------------
#'@export
get_exp_plotly <- function(study, norm, gene) {
  conn <- db_utils$get_db_connection()
  table_name <- paste0(study, "_", tolower(norm))

  gg_obj <- tbl(conn, table_name) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    filter(gene_name == gene) %>%
    select(gene_name, contains("SRR")) %>%
    collect() %>%
    pivot_longer(contains("SRR")) %>%
    select(-gene_name) %>%
    rename(sample_id = name) %>%
    left_join(
      tbl(conn, "metadata") %>% select(sample_id, condition) %>% collect(),
      by = "sample_id"
    ) %>%
    ggplot(aes(text = sample_id, x = condition, y = value, fill = condition)) +
    geom_boxplot(width = .65, alpha = .6, outlier.shape = NA) +
    geom_jitter(width = .15) +
    xlab("Conditions of Samples") +
    ylab(paste0("Expression (", norm, ")")) +
    theme_gray(base_size = 13) +
    ggtitle(gene) +
    theme(legend.position = "none")

  DBI$dbDisconnect(conn)

  return(plotly$ggplotly(gg_obj, tooltip = c("text", "x", "y")))
}


# --- VOLCANO PLOT -------------------------------------------------------------
#'@export
get_volcano <- function (study, contrast, gene) {
  conn <- db_utils$get_db_connection()
  table_name <- paste0(study,"_","degs")
  pair <- strsplit(contrast, " vs. ")[[1]]

  degs <- tbl(conn, table_name) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    collect() %>%
    filter(numerator == pair[[1]] & denominator == pair[[2]])

  plot_title <- paste0(pair[1], " vs. ", pair[2])
  plotdata <- degs %>%
    mutate(
      hlight = gene_name == gene,
      FDR = case_when(
        FDR == 0 ~ .Machine$double.xmin, TRUE ~ FDR
      ),
      FDR = -log10(FDR),
      sigcond = case_when(
        FDR < 2 ~ "n.s.",
        abs(logFC) < 1 ~ "sig-only",
        logFC > 1 ~ "Over-expressed",
        logFC < -1 ~ "Under-expressed"
      )
    ) %>%
    arrange(hlight, desc(FDR))

  maxval <- max(plotdata$FDR)

  #plot
  volcano_plot <- plotdata %>%
    ggplot(
      aes(x = logFC, y = FDR, color = sigcond, size = hlight)
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", alpha = .25) +
    geom_vline(xintercept = -1, linetype = "dashed", alpha = .25) +
    geom_hline(yintercept = 3, linetype = "dashed", alpha = .25) +
    geom_point() +
    xlab("Log2 Fold Change") +
    ylab("-log10(Adjusted p-value)") +
    guides(size = guide_none(),
           color = guide_legend(title = NULL)) +
    theme_bw(base_size = 18) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 1.05 * maxval)) +
    ggtitle(plot_title, subtitle = gene) +
    scale_color_manual(
      values = c(
        "n.s." = "#d6d6d6",
        "sig-only" = "#91bac4",
        "Over-expressed" = "#2fa4c2",
        "Under-expressed" = "#c24e2f"
      )
    ) +
    theme(legend.position = "bottom", legend.text = element_text(size = 16))

  DBI$dbDisconnect(conn)
  return(volcano_plot)
}


# --- HEATMAP ------------------------------------------------------------------
#'@export
get_heatmap <- function(study, contrast, norm) {
  conn <- db_utils$get_db_connection()
  deg_table <- paste0(study,"_","degs")
  norm_table <- paste0(study, "_", tolower(norm))
  pair <- strsplit(contrast, " vs. ")[[1]]

  num <- pair[1]
  dem <- pair[2]

  plotted_genes <- tbl(conn, deg_table) %>%
    filter(numerator == num & denominator == dem) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    mutate(gene_name = case_when(is.na(gene_name) ~ gene_id,TRUE ~ gene_name
    )) %>%
    mutate(sigcond = case_when(
      FDR > 0.05 ~ "n.s.",
      abs(logFC) < 1 ~ "sig-only",
      logFC > 1 ~ "Over-expressed",
      logFC < -1 ~ "Under-expressed"
    )) %>%
    filter(sigcond %in% c("Over-expressed", "Under-expressed")) %>%
    group_by(sigcond) %>%
    slice_min(order_by = FDR, n = 12) %>%
    collect() %>%
    pull (gene_name)


  values_table <- tbl(conn, norm_table) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    filter(gene_name %in% plotted_genes) %>%
    pivot_longer(contains("SRR")) %>%
    rename(sample_id = name) %>%
    collect() %>%
    left_join(
      tbl(conn, "metadata") %>% select(sample_id, condition) %>% collect(),
      by = "sample_id"
    ) %>%
    filter(condition == pair[[1]] | condition == pair[[2]])

  mat <- pivot_wider(
    data = values_table,
    id_cols = gene_name, names_from = sample_id, values_from = value
    ) %>%
    column_to_rownames("gene_name") %>%
    as.matrix()

  annot <- values_table %>%
    select(sample_id, condition) %>%
    unique() %>%
    column_to_rownames("sample_id")

  ann_colors <- c("red", "blue")
  names(ann_colors) <- annot %>% pull(condition) %>% unique()
  ann_colors <- list(condition = ann_colors)
  plot_title <- paste0(pair[1], " vs. ", pair[2])

heatmap_obj <- ComplexHeatmap$pheatmap(mat,
           scale = "row",
           angle_col = "90",
           annotation_col = annot,
           annotation_colors = ann_colors,
           name = norm,
           main = plot_title)

  DBI$dbDisconnect(conn)
  return(heatmap_obj)
}


# --- PATHWAY ANALYSIS: ENRICH PLOT --------------------------------------------
#'@export
get_enrich_plot <- function(study, contrast, metric) {
  conn <- db_utils$get_db_connection()
  study_contrast <- paste0(study, "_", gsub(" vs. ", "_", contrast))

  terms <- tbl(conn, "enrichr") %>%
    filter(Study_Contrast == study_contrast) %>%
    collect() %>%
    group_by(group) %>%
    slice_max(Combined.Score, n = 10) %>%
    pull(Term)

  mat <- tbl(conn, "enrichr") %>%
    filter(Study_Contrast == study_contrast) %>%
    filter(Term %in% terms) %>%
    mutate(`Padj (-log10)`=-log10(Adjusted.P.value)) %>%
    rename(metric = contains(metric)) %>%
    collect() %>%
    pivot_wider(
      id_cols = Term, names_from = group,
      values_from = metric,
      values_fill = 0
    ) %>%
    column_to_rownames("Term") %>%
    as.matrix()

  DBI$dbDisconnect(conn)

  heatmap_obj <- ComplexHeatmap$pheatmap(
    mat,
    name = metric,
    angle_col = "0",
  )

  DBI$dbDisconnect(conn)
  return(heatmap_obj)
}


# --- COMPARISON: UPSET PLOT ---------------------------------------------------
#'@export
get_upset_plot <- function(study, deg_type) {
  conn <- db_utils$get_db_connection()
  table_name <- paste0(study, "_degs")

  tolist <- tbl(conn, table_name) %>%
    filter(!is.na(FDR) & FDR < .01 & abs(logFC) > 1) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    mutate(gene_name = case_when(
      is.na(gene_name) ~ gene_id,
      TRUE ~ gene_name
    )) %>%
    mutate(
      group = case_when(
        logFC > 0 ~ "Over-expressed",
        TRUE ~ "Under-expressed"
      ),
      set_name = paste0(study, "\n", numerator, " vs. ", denominator)
    ) %>%
    filter(group == deg_type) %>%
    select(gene_name, set_name) %>%
    collect()

  all_set_names <- tolist %>% pull(set_name) %>% unique()
  names(all_set_names) <- all_set_names
  set2genes <- lapply(all_set_names, function(set_name){
    tolist %>%
      filter(set_name == !!set_name) %>%
      pull(gene_name)
  })

  m <- ComplexHeatmap$make_comb_mat(set2genes)
  plot_obj <- ComplexHeatmap$UpSet(m, row_names_gp = grid$gpar(fontsize = 10))

  DBI$dbDisconnect(conn)

  return(plot_obj)
}
