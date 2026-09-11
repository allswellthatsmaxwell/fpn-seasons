library(ggplot2)
library(tibble)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)
library(lubridate)
library(assertthat)
library(glue)

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

getDataPaths <- function(data_dir, filter_seasons) {
  assert_that(all(filter_seasons %in% VALID_SEASONS), 
              msg = glue("invalid season found in filter_seasons: '{filter_seasons}'"))
  paths <- list.files(data_dir) %>%
    .[str_detect(., filter_seasons)] %>%
    sapply(. %>% paste0(data_dir, .))
  assert_that(length(paths) > 0, 
              msg = glue("getDataPaths: didn't find any seasons matching ", 
                         "filter_seasons '{filter_seasons}'"))
  paths
}

getPointsOverSeasonFrame <- function(path) {
  season <- str_extract(path, "(winter|spring|summer|fall)-\\d{4}")
  assert_that(!is.na(season),
              msg = glue("could not extract season name from path {path}"))
  assert_that(length(path) == 1)
  readr::read_csv(path) %>%
    dplyr::select(-`Total Points`) %>%
    tidyr::pivot_longer(!Player, names_to = "date", values_to = "points") %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(date = paste0(date, "/2026"),
                  date = lubridate::mdy(date)) %>%
    dplyr::arrange(player, date) %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(points_so_far = cumsum(points),
                  season = season)
}

# getPOIFrame <- function(players_of_interest) {
#   lapply(names(players_of_interest),
#          function(season) {
#            players <- players_of_interest[[season]]
#            lapply(players, function(player) data.frame(season, player)) %>%
#              dplyr::bind_rows()
#          }) %>%
#     dplyr::bind_rows()
# }


DATA_DIR <- "data/"
PLAYERS_OF_INTEREST_SEASONAL <-
    list("summer-2026" = c("Maxwell Peterson", "Paul Gonzalez"),
         "spring-2026" = c("Maxwell Peterson", "Jeremiah Stene"),
         "winter-2026" = c("Maxwell Peterson", "Anna Kinkead"))

VALID_SEASONS <- names(PLAYERS_OF_INTEREST_SEASONAL)


setwd('fpn-analysis/')


paths <- getDataPaths(DATA_DIR, "summer-2026")
dat <- getPointsOverSeasonFrame(paths) %>%
  mutate(of_interest = player %in% PLAYERS_OF_INTEREST_SEASONAL[season][[1]])
assertSameWeeksForEveryone(dat)


dat %>%
  ggplot(aes(x = date, y = points_so_far, 
             group = player, color = of_interest)) +
  geom_line() +
  scale_x_date(breaks = "week", date_labels = "%B %d") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "#C1C1C1")) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank())

