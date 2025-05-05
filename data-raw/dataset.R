
packages <- c("dplyr")

installed_packages <- packages %in% rownames(installed.packages())

if(any(installed_packages == FALSE)) {
  
  install.packages(packages[!installed_packages])
  
}

invisible(lapply(packages, library, character.only = TRUE))

nfcs_raw_data <- read.csv("./data-raw/NFCS 2021 State Data 220627.csv",
                   header = TRUE,
                   sep = ",",
                   na.strings = c(""),
                   stringsAsFactors = FALSE)

save(nfcs_raw_data, file = "./data/nfcs_raw_data.Rdata")

