library(tidyverse)
library(jsonlite)
library(tidyverse)
library(gt)
library(furrr)
library(glue)
library(lubridate)
plan(multiprocess(workers = parallel::detectCores() - 1))
source('helpers.R') 

df_start <- 
  tibble('matchup_id' = 1:20) %>% 
  mutate('start_cap' = case_when(matchup_id == 1 ~ 13, 
                                 T ~ 8),
         'duration' = case_when(matchup_id == 1 ~ 11, 
                                T ~ 7),
         'end_period' = cumsum(duration),
         'start_period' = end_period - duration + 1)

df_schedule <- read_csv('info/schedule.csv')

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
               'starts' = sum(.x$stats$`33`, na.rm = T))
      } else {
        tibble('points' = 0,
               'starts' = 0)
      } }) %>% 
    mutate('player' = y$entries[[index]]$playerPoolEntry$player$fullName,
           'player_id' = y$entries[[index]]$playerPoolEntry$player$id) %>% 
    mutate('team_id' = ifelse(team == 'home', df_schedule$home_team_id[index], df_schedule$away_team_id[index]))
  
  return(df)
  
}


get_matchup_stats <- function(week) {
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]
  indices <- seq(1 + 6 * (week-1), 6 * week, 1)
  
  df <- 
    map_dfr(start:end, ~{
      x <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/2022/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchupScore'))
      y <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/2022/segments/0/leagues/49106?scoringPeriodId={.x}&view=mMatchup'))
      z <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/2022/segments/0/leagues/49106?scoringPeriodId={.x}&view=mRoster'))
      
      roster_status <- 
        map2_dfr(z$teams$roster$entries, z$teams$id, ~{
          tibble('player' = .x$playerPoolEntry$player$fullName,
                 'player_id' = .x$playerPoolEntry$player$id,
                 'lineup_id' = .x$lineupSlotId,
                 'team_id' = .y)
        }) 
      
      df <- 
        bind_rows(map_dfr(indices, function(i) get_daily_stats(x, y, i, 'home')),
                  map_dfr(indices, function(i) get_daily_stats(x, y, i, 'away'))) %>% 
        mutate('scoring_period_id' = .x) %>% 
        mutate('matchup_id' = week) %>% 
        left_join(roster_status, by = c("player", "team_id"))                                                                                                                        
      
      df
      
    })
  
  return(df)
}

df <- map_dfr(1:2, get_matchup_stats)
df <- bind_rows(df, get_matchup_stats(3))

write(df, 'history/daily_stats_2022.csv')


df %>% 
  group_by(team_id, lineup_id) %>% 
  summarise('ppg' = mean(points)) %>% 
  filter(lineup_id == 1)
  summarise('points' = sum(points),
            'starts' = sum(starts))


z <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/2022/segments/0/leagues/49106?scoringPeriodId={.x}&view=mRoster'))


