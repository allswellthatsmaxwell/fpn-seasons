library(ggplot2)
library(tibble)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(lubridate)
library(assertthat)

assertSameWeeksForEveryone <- function(dat) {
  unique_weekcounts <- dat %>%
    dplyr::group_by(player) %>%
    dplyr::summarise(nweeks = n()) %>%
    dplyr::group_by(nweeks) %>%
    dplyr::summarise(n_players = n())
  assert_that(nrow(unique_weekcounts) == 1, 
              msg = "in assertSameWeeksForEveryone: Some players appear to have
a different number of weeks listed than others.")
}

setwd('fpn-analysis/')

DATA_DIR <- "data/"
paths <- list.files(DATA_DIR) %>%
  .[str_detect(., "summer")] %>%
  sapply(. %>% paste0(DATA_DIR, .))
paths

dat <- read_csv(paths) %>%
  dplyr::select(-`Total Points`) %>%
  tidyr::pivot_longer(!Player, names_to = "date", values_to = "points") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(date = paste0(date, "/2026"),
                date = lubridate::mdy(date))

assertSameWeeksForEveryone(dat)

mdy(c("06/17/1990", "06/17/1990"))
