box::use(
  DBI,
  dplyr[...],
  RSQLite[SQLite],
  prompter[add_prompt],
  shiny[...],
)


# Get a connection to the Sqlite DB
#'@export
get_db_connection <- function() {
  DBI$dbConnect(RSQLite::SQLite(), "data.sqlite")
}


# Get contrasts for a given study
#'@export
get_contrasts <- function(study) {
  conn <- get_db_connection()

  contrasts <- tbl(conn, "contrasts") %>%
    filter(study_id == study) %>%
    mutate(contrast = paste0(numerator, " vs. ", denominator)) %>%
    pull(contrast) %>%
    sort()

  DBI$dbDisconnect(conn)

  return(contrasts)
}


# Get DEGs as a dataframe for a given study and contrast
#'@export
get_deg_df <- function(study, contrast) {
  contrasts <- strsplit(contrast, " vs. ")[[1]]
  numerator <- contrasts[1]
  denominator <- contrasts[2]

  conn <- get_db_connection()

  table_name <- paste0(study, "_degs")

  df <- tbl(conn, table_name) %>%
    filter(numerator == !!numerator) %>%
    filter(denominator == !!denominator) %>%
    left_join(tbl(conn, "ens2sym"), by = "gene_id") %>%
    # filter(!is.na(gene_name)) %>%
    mutate(gene_name = case_when(
      is.na(gene_name) ~ gene_id,
      TRUE ~ gene_name
    )) %>%
    select(gene_name, logFC, FDR) %>%
    arrange(FDR) %>%
    collect()

  DBI$dbDisconnect(conn)

  return(df)
}


# Make headers
#'@export
makeHeaders <- function(title, message, fs=1.3) {
  tagList(
    span(span(title, style=paste0("font-size: ", fs, "em;"))
         )
  )
}

