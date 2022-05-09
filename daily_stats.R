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

get_daily_stats <- function(x, y, index, team, df_schedule) {
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
               'relief' = as.logical(sum(.x$stats$`32` > 0 & .x$stats$`33` == 0, na.rm = T)),
               'relief_start' = as.logical(sum(.x$stats$`33` == 0 & .x$stats$`34` >= 12, na.rm = T)),
               'qs' = as.logical(sum(.x$stats$`63`, na.rm = T)),
               'save' = as.logical(sum(.x$stats$`57`, na.rm = T)))
      } else {
        tibble('points' = 0,
               'start' = F,
               'relief' = F,
               'relief_start' = F,
               'qs' = F,
               'save' = F)
               
      } }) %>% 
    mutate('player' = y$entries[[index]]$playerPoolEntry$player$fullName,
           'player_id' = y$entries[[index]]$playerPoolEntry$player$id) %>% 
    mutate('team_id' = ifelse(team == 'home', df_schedule$home_team_id[index], df_schedule$away_team_id[index])) %>% 
    mutate('game_id' = index)
  
  return(df)
  
}


get_matchup_stats <- function(week, season = 2022) {
  
  df_schedule <- read_csv(glue('data/stats/{season}/schedule_{season}.csv'))
  
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]
  
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
      
      tmp1 <- map_dfr(indices, function(i) get_daily_stats(x, y, i, 'home', df_schedule))
      tmp2 <- map_dfr(indices, function(i) get_daily_stats(x, y, i, 'away', df_schedule))
      if(nrow(tmp1) > 0) {
        df <- 
          bind_rows(tmp1, tmp2) %>% 
          mutate('scoring_period_id' = .x) %>% 
          mutate('matchup_id' = week) %>% 
          left_join(roster_status, by = c("player", "player_id", "team_id")) %>% 
          mutate('in_lineup' = lineup_id <= 15,
                 'pitcher' = lineup_id %in% c(14, 15),
                 'batter' = lineup_id <= 12) %>% 
          mutate('season' = season)
        
        
        df
      } else {
        NULL
      }
      
    })
  
  return(df)
}

# ### 2021 Scrape
# df_2021 <- map_dfr(1:20, ~get_matchup_stats(.x, season = 2021))
# write_csv(df_2021, 'data/stats/2021/daily_stats_2021.csv')

# # 2022 Scrape
# df_2022 <- map_dfr(1:4, ~get_matchup_stats(.x, season = 2022))
# write_csv(df_2022, 'data/stats/2022/daily_stats_2022.csv')
