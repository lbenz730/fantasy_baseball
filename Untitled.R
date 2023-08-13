library(tidyverse)
library(xgboost)
library(furrr)
plan(multisession, workers = 12)

source('figures/wp_graphics.R')
df_start <- 
  read_csv('data/df_start.csv') %>% 
  filter(season > 2019) %>% 
  filter(season < 2023 | matchup_id < 18, !playoffs)


tmp_f <- function(m,s) {
  cat(m, s, '\n')
  x <- 
    plot_wp(season = s,
            plot = F, all = T,
            week = m) %>% 
    group_by_at(vars(contains('team'))) %>% 
    summarise('winner' = ifelse(last(win_prob) == 0, team_away, team_home),
              'loser' = ifelse(last(win_prob) == 1, team_away, team_home),
              'min_prob' = ifelse(last(win_prob) == 1, min(win_prob), min(1-win_prob))) %>% 
    mutate('matchup_id' = m,
           'season' = s) %>% 
    ungroup() %>% 
    select(-contains('team'))
  
  return(x)
}

x <- future_map2_dfr(df_start$matchup_id, df_start$season, tmp_f, .progress = T)


