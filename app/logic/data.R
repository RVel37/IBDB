GEO_BASE <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="
GENECARDS_BASE <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
S3_HTTPS <- "https://liverdb-data.s3.amazonaws.com/"

app_data <- readRDS("app/data/app_data.rds")

exps <- app_data[["exps"]]
exps126 <- exps[["GSE126848"]]
exps135 <- exps[["GSE135251"]]

degs <- app_data[["degs"]]
degs126 <- degs[["GSE126848"]]
degs135 <- degs[["GSE135251"]]

metadata <- app_data[["metadata"]]

deg_contrasts <- app_data[["contrasts"]]

eres <- app_data[["eres"]]
