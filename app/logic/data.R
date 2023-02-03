
GEO_BASE <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="
GENECARDS_BASE <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
S3_HTTPS <- "https://liverdb-data.s3.amazonaws.com/"

app_data <- readRDS("app/app_data.rds")

exps <- app_data[["exps"]]
degs <- app_data[["degs"]]
metadata <- app_data[["metadata"]]
deg_contrasts <- app_data[["contrasts"]]
eres <- app_data[["eres"]]
