setwd('~/fpn-analysis/points-over-time')

library(ggplot2)
library(dplyr)
library(magrittr)

FPATH <- "data/maxwell_peterson.psv"
#FPATH <- "data/cedric_thompson.psv"
#FPATH <- "data/david_lindgren.psv"
dat <- read.csv(FPATH, sep="|") %>%
  mutate(date = as.Date(date, "%B %d, %Y"),
         tournament_number = 1:length(date),
         cumulative_points = cumsum(points),
         points_per_tournament = cumulative_points / tournament_number)
  
filterBreaks <- function(dat, n) {
  dat %>%
    dplyr::filter(tournament_number %% n == 0)
}

break_n <- 7

dat %>%
  dplyr::slice(-1) %>%
  ggplot(aes(x=tournament_number, y=points_per_tournament)) +
  scale_x_continuous(
    breaks = filterBreaks(dat, break_n) %$% tournament_number,
    labels = filterBreaks(dat, break_n) %$% format(date, "%b %d, %Y")) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=365-45, hjust=0, vjust=1),
        axis.title.x = element_blank())

