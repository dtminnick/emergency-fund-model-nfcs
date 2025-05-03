
packages <- c("dplyr")

installed_packages <- packages %in% rownames(installed.packages())

if(any(installed_packages == FALSE)) {
  
  install.packages(packages[!installed_packages])
  
}

invisible(lapply(packages, library, character.only = TRUE))

source <- read.csv("./data-raw/NFCS 2021 State Data 220627.csv",
                   header = TRUE,
                   sep = ",",
                   na.strings = c(""),
                   stringsAsFactors = FALSE)

save(source, file = "./data/source.Rdata")

