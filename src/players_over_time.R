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

getDataPaths <- function(data_dir, filter_str) {
  paths <- list.files(data_dir) %>%
    .[str_detect(., filter_str)] %>%
    sapply(. %>% paste0(data_dir, .))
}

getPointsOverSeasonDat <- function(paths) {
  readr::read_csv(paths) %>%
    dplyr::select(-`Total Points`) %>%
    tidyr::pivot_longer(!Player, names_to = "date", values_to = "points") %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(date = paste0(date, "/2026"),
                  date = lubridate::mdy(date)) %>%
    dplyr::arrange(player, date) %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(points_so_far = cumsum(points))
}

setwd('fpn-analysis/')

DATA_DIR <- "data/"
paths <- getDataPaths(DATA_DIR, "summer")
dat <- getPointsOverSeasonDat(paths)
assertSameWeeksForEveryone(dat)

players_of_interest <- c("Maxwell Peterson", "Paul Gonzalez")
dat %<>% mutate(of_interest = any(player == players_of_interest))

dat %>%
  ggplot(aes(x = date, y = points_so_far, 
             group = player, color = of_interest)) +
  geom_line() +
  scale_x_date(breaks = "week", date_labels = "%B %d") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "#C1C1C1")) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank())

