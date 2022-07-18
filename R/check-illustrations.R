library(tidyverse)

check.file <- function(filename) {
  status <- file.exists(file.path("images", filename))
  
  if (!status) {
    cat(paste("Missing", filename, "\n"))
  }
  
  status
}

check.illustrations <- function(df) {
  lapply(df$illustration, check.file)
  return(NULL)
}

read_csv('data/scoring.csv') |>
  check.illustrations()

explore <- read_csv('data/explore.csv') |>
  check.illustrations()

ambush <- read_csv('data/ambush.csv') |>
  check.illustrations()
