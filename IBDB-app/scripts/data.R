raw <- readRDS("./scripts/raw_data/app_data.rds")

exps <- raw[["exps"]]
  saveRDS(exps, "exps.rds")
  
degs <- raw[["degs"]]
  saveRDS(degs, "degs.rds")

metadata <- raw[["metadata"]]
  saveRDS(degs135, "degs135.rds")

deg_contrasts <- raw[["contrasts"]]
  saveRDS(deg_contrasts, "deg_contrasts.rds")

eres <- raw[["eres"]]
  saveRDS(eres, "eres.rds")


#move RDS files into target folder (app/data)

rds_files <- list.files(pattern = "*.rds")

for (file in rds_files) {
  file.rename(file, file.path("app/data", basename(file)))
}
