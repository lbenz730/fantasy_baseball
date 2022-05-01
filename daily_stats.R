library(tidyverse)
library(jsonlite)
library(tidyverse)
library(gt)
library(furrr)
library(glue)
library(lubridate)
plan(multiprocess(workers = parallel::detectCores() - 1))
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2000)
source('helpers.R') 

df_start <- 
  tibble('matchup_id' = 1:20) %>% 
  mutate('start_cap' = case_when(matchup_id == 1 ~ 13,
                                 matchup_id == 14 ~ 11,
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 11, 
                                matchup_id == 14 ~ 14,
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1)

df_schedule <- read_csv('info/schedule_2022.csv')

get_daily_stats <- function(x, y, index, team) {
  if(team == 'home') {
    x <- x$schedule$home$rosterForCurrentScoringPeriod
    y <- y$schedule$home$rosterForCurrentScoringPeriod
  } else {
    x <- x$schedule$away$rosterForCurrentScoringPeriod
    y <- y$schedule$away$rosterForCurrentScoringPeriod
  }
  
  df <- 
    map_dfr(x$entries[[index]]$playerPoolEntry$player$stats, ~{
      if(length(.x) > 0) {
        tibble('points' = sum(.x$appliedTotal, na.rm = T),
               'start' = as.logical(sum(.x$stats$`33` > 0 | .x$stats$`34` >= 12, na.rm = T)),
               'relief_start' = as.logical(sum(.x$stats$`33` == 0 & .x$stats$`34` >= 12, na.rm = T)))
      } else {
        tibble('points' = 0,
               'start' = F,
               'relief_start' = F)
      } }) %>% 
    mutate('player' = y$entries[[index]]$playerPoolEntry$player$fullName,
           'player_id' = y$entries[[index]]$playerPoolEntry$player$id) %>% 
    mutate('team_id' = ifelse(team == 'home', df_schedule$home_team_id[index], df_schedule$away_team_id[index]))
  
  return(df)
  
}


get_matchup_stats <- function(week, end_early = F, season = 2022) {
  if(season == 2021) {
    df_schedule <- read_csv('history/schedule_2021.csv') 
  }
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]
  if(end_early) {
    end <- start + wday(Sys.Date()-2) 
  }
  
  df <- 
    future_map_dfr(start:end, ~{
      x <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchupScore'))
      y <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchup'))
      z <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/{season}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mRoster'))
      
      roster_status <- 
        map2_dfr(z$teams$roster$entries, z$teams$id, ~{
          tibble('player' = .x$playerPoolEntry$player$fullName,
                 'player_id' = .x$playerPoolEntry$player$id,
                 'lineup_id' = .x$lineupSlotId,
                 'team_id' = .y)
        }) 
      
      indices <- which(!map_lgl(x$schedule$home$rosterForCurrentScoringPeriod$entries, is.null))
      
      df <- 
        bind_rows(map_dfr(indices, function(i) get_daily_stats(x, y, i, 'home')),
                  map_dfr(indices, function(i) get_daily_stats(x, y, i, 'away'))) %>% 
        mutate('scoring_period_id' = .x) %>% 
        mutate('game_id' = z$gameId) %>% 
        mutate('matchup_id' = week) %>% 
        left_join(roster_status, by = c("player", "player_id", "team_id")) %>% 
        mutate('in_lineup' = lineup_id <= 15,
               'pitcher' = lineup_id %in% c(14, 15),
               'batter' = lineup_id <= 9)
      
      
      df
      
    })
  
  return(df)
}

df_2021 <- map_dfr(1:20, ~get_matchup_stats(.x, season = 2021))

# df1 <- get_matchup_stats(1)
# df2 <- get_matchup_stats(2)
df3 <- get_matchup_stats(3, T)
# 
# 
# bind_rows(df1, df2, df3) %>% 
#   write_csv('history/daily_stats_2022.csv')
# 
# 
# df %>% 
#   group_by(team_id, lineup_id) %>% 
#   summarise('ppg' = mean(points)) %>% 
#   filter(lineup_id == 1)
# summarise('points' = sum(points),
#           'starts' = sum(starts))
# 
# 
# 
# df %>% 
#   filter(in_lineup) %>% 
#   inner_join(df_start, by = 'matchup_id') %>% 
#   mutate('day_of_matchup' = scoring_period_id - start_period + 1) %>% 
#   group_by(matchup_id, team_id, day_of_matchup, start_cap) %>% 
#   summarise('day_points' = sum(points),
#             'start_points' = sum(points[start], na.rm = T),
#             'starts' = sum(start)) %>% 
#   group_by(matchup_id, team_id) %>% 
#   mutate('total_points' = cumsum(day_points),
#          'total_starts' = cumsum(starts)) %>% 
#   View()


# 



read_csv('history/daily_stats_2022.csv') %>% 
  filter(matchup_id < 3) %>% 
  bind_rows(df3) %>% 
  filter(in_lineup) %>%
  # filter(scoring_period_id == max(scoring_period_id)) %>%
  group_by(team_id) %>% 
  summarise('points' = sum(points)) %>% 
  ungroup() %>% 
  mutate('pts_back' = points - max(points)) %>% 
  arrange(-points) %>% 
  inner_join(teams)

