
GEO_BASE <- "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc="
GENECARDS_BASE <- "https://www.genecards.org/cgi-bin/carddisp.pl?gene="
S3_HTTPS <- "https://liverdb-data.s3.amazonaws.com/"

app_data <- readRDS("app/data/app_data.rds")

exps1 <- app_data[["exps"]]
exps126 <- exps1[["GSE126848"]]
exps135 <- exps1[["GSE135251"]]

degs1 <- app_data[["degs"]]
degs126 <- degs1[["GSE126848"]]
degs135 <- degs1[["GSE135251"]]

metadata1 <- app_data[["metadata"]]

deg_contrasts <- app_data[["contrasts"]]

eres1 <- app_data[["eres"]]
