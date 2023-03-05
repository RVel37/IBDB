library(data.table)
library(dplyr)

gse112 <- readLines('gse112.txt')
gse123 <- readLines('gse123.txt')
gse836 <- readLines('gse836.txt')

#-----------------------------
#extract labels, SRR IDs, and groupings from text file 
# (FOR GSE112057)

gse112cut <- as.data.frame (gse112[grepl('###*|paste SRR*|colnames(x)*', gse112)])
colnames(gse112cut) <- c("GSE112057")

row_names <- c()
#rename rows
for (letter in letters[1:20]) {
  for (number in 1:3) {
    #combine letter and number to create row name
    row_name <- paste0(letter, number)
    # add row name to vector
    row_names <- c(row_names, row_name)
  }
}

rownames(gse112cut) <- row_names[1:45]

#-----------------------------
#apply regex to return DF with run IDs + associated conditions

# initialise output list
outputlist <- list()

#loop through each group of 3 rows
for (letter in letters[1:15]) {
  
  #get ID column for a group
  start <- paste0(letter,"2")
  #get condition column for a group
  end <- paste0(letter,"3")
  
  #extract IDs + conditions with regex
  id_vec <- unlist(stringr::str_extract_all
                   (gse112cut[start, "GSE112057"], "SRR\\d{7}"))
  
  con_str <- gsub('.*\\("(.*)"\\)', '\\1', gse112cut[end, "GSE112057"])
  con_vec <- unlist(strsplit(con_str, '","'))
  con_vec <- gsub ("[[:digit:]\\.]+", "",con_vec)
  
  #combine vectors into new DF
  df <- data.frame(SRR_ID = id_vec, condition = con_vec)
  #create list  
  outputlist[[letter]] <- df
}

#turn into DF
output112 <- rbindlist(outputlist)
colnames(output112) <- c("SRR_ID","condition")
#delete duplicate SRR IDs from output
output112 <- unique(output112, by="SRR_ID")



#-------------------------------------------
#Repeat above for gse123 + gse836:

# GSE123141

gse123cut <- as.data.frame (gse123[grepl('###*|paste SRR*|colnames(x)*', gse123)])
colnames(gse123cut) <- c("GSE123141")

row_names <- c()
for (letter in letters[1:4]) {
  for (number in 1:3) {
    row_name <- paste0(letter, number)
    row_names <- c(row_names, row_name)
  }
}
rownames(gse123cut) <- row_names[1:12]

outputlist <- list()
for (letter in letters[1:4]) {
  start <- paste0(letter,"2")
  end <- paste0(letter,"3")
  
  id_vec <- unlist(stringr::str_extract_all
                   (gse123cut[start, "GSE123141"], "SRR\\d{7}"))
  con_str <- gsub('.*\\("(.*)"\\)', '\\1', gse123cut[end, "GSE123141"])
  con_vec <- unlist(strsplit(con_str, '","'))
  con_vec <- gsub ("[[:digit:]\\.]+", "",con_vec)
  
  df <- data.frame(SRR_ID = id_vec, condition = con_vec)
  outputlist[[letter]] <- df
}

output123 <- rbindlist(outputlist)
colnames(output123) <- c("SRR_ID","condition")
output123 <- unique(output123, by="SRR_ID")



# GSE83687

gse836cut <- as.data.frame (gse836[grepl('###*|paste SRR*|colnames(x)*', gse836)])
colnames(gse836cut) <- c("GSE83687")

row_names <- c()
for (letter in letters[1:4]) {
  for (number in 1:3) {
    row_name <- paste0(letter, number)
    row_names <- c(row_names, row_name)
  }
}
rownames(gse836cut) <- row_names[1:12]

outputlist <- list()
for (letter in letters[1:4]) {
  start <- paste0(letter,"2")
  end <- paste0(letter,"3")
  
  id_vec <- unlist(stringr::str_extract_all
                   (gse836cut[start, "GSE83687"], "SRR\\d{7}"))
  con_str <- gsub('.*\\("(.*)"\\)', '\\1', gse836cut[end, "GSE83687"])
  con_vec <- unlist(strsplit(con_str, '","'))
  con_vec <- gsub ("[[:digit:]\\.]+", "",con_vec)
  
  df <- data.frame(SRR_ID = id_vec, condition = con_vec)
  outputlist[[letter]] <- df
}

output836 <- rbindlist(outputlist)
colnames(output836) <- c("SRR_ID","condition")
output836 <- unique(output836, by="SRR_ID")


#-------------------------------------------
#Add strandedness

output123 <- output123 %>% mutate (strandedness = "unstranded")
output112 <- output112 %>% mutate (strandedness = "reverse")
output836 <- output836 %>% mutate (strandedness = "reverse")

outputall <- rbind(output112,output123, output836)
colnames(outputall) <- c("sample_id", "condition","strandedness")

#-------------------------------------------
#Join onto metadata.csv
 
metadata <- read.csv('metadata.csv')
metadata <- left_join(metadata, outputall, by='sample_id')

write.csv(metadata, file='metadata.csv', row.names=TRUE)


