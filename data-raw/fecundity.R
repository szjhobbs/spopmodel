# raw data for White Sturgeon fecundity

FecundityWST <- read.csv(file = "data-raw/fecundity.csv", header = TRUE)

devtools::use_data(FecundityWST, overwrite = TRUE)
