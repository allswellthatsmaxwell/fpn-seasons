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

assertSameWeeksForEveryoneMultiSeason <- function(dat) {
  dat %>%
    split(.$season) %>%
    lapply(assertSameWeeksForEveryone) %>%
    invisible()
}

getDataPaths <- function(data_dir, filter_seasons) {
  assert_that(all(filter_seasons %in% VALID_SEASONS), 
              msg = glue("invalid season found in filter_seasons: '{filter_seasons}'"))
  paths <- list.files(data_dir) %>%
    .[sapply(., function(s) s %>% str_detect(filter_seasons) %>% any())] %>%
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
  
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::select(-`Total Points`) %>%
    tidyr::pivot_longer(!Player, names_to = "date", values_to = "points") %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(date = paste0(date, "/2026"),
                  date = lubridate::mdy(date)) %>%
    dplyr::arrange(player, date) %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(points_so_far = cumsum(points),
                  season = season) %>%
    addWeekNumberColumn()
}

addWeekNumberColumn <- function(dat) {
  orig_rowcount <- nrow(dat)
  result_dat <- dat %>% 
    ungroup() %>%
    select(date) %>% 
    unique() %>% 
    mutate(week_number = 1:n()) %>%
    inner_join(dat, by = c("date"))
  
  assert_that(nrow(result_dat) == orig_rowcount)
  
  result_dat
}


getSeasonsFrame <- function(paths) {
  paths %>%
    lapply(getPointsOverSeasonFrame) %>%
    dplyr::bind_rows() %>%
    mutate(of_interest = player %in% PLAYERS_OF_INTEREST_SEASONAL[season][[1]],
           coloring = if_else(of_interest, POI_COLORS[player], NON_POI_COLOR))
}




DATA_DIR <- "data/"
PLAYERS_OF_INTEREST_SEASONAL <-
    list("summer-2026" = c("Maxwell Peterson", "Paul Gonzalez"),
         "spring-2026" = c("Maxwell Peterson", "Jeremiah Stene"),
         "winter-2026" = c("Maxwell Peterson", "Anna Kinkead", "Jesse Reiter"))

POI_COLORS <- c("Maxwell Peterson" = "#FFC125",
                "Jeremiah Stene" = "#000000",
                "Anna Kinkead" = "#8B4513",
                "Jesse Reiter" = "#388E8E",
                "Paul Gonzalez" = "#FF83FA")
NON_POI_COLOR = "#C1C1C1"

VALID_SEASONS <- names(PLAYERS_OF_INTEREST_SEASONAL)


setwd('~/fpn-analysis/')


paths <- getDataPaths(DATA_DIR, VALID_SEASONS)
paths <- getDataPaths(DATA_DIR, "winter-2026")

dat <- getSeasonsFrame(paths)
dat

assertSameWeeksForEveryoneMultiSeason(dat)


ggplot(mapping = aes(x = date, y = points_so_far, 
                     group = player, color = coloring, 
                     linewidth = of_interest, alpha = of_interest)) +
  geom_line(data = filter(dat, !of_interest)) +
  geom_line(data = filter(dat, of_interest)) +
  scale_x_date(breaks = "week", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  scale_color_identity() +
  scale_linewidth_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  facet_wrap(~season, ncol = 1) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank())

