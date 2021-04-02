# raw data for White Sturgeon spawning probability

SpawningProbWST <- read.csv(file = "data-raw/spawning.csv", header = TRUE)

devtools::use_data(SpawningProbWST, overwrite = TRUE)
