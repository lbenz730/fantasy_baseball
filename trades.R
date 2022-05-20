library(tidyverse)
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

get_trades <- function(week) {
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]
  
  df <- 
    future_map_dfr(start:end, ~{
      w <- robust_scrape(glue('https://fantasy.espn.com/apis/v3/games/flb/seasons/2022/segments/0/leagues/49106?scoringPeriodId={.x}&view=mTransactions2'))
      w %>% 
        pluck('transactions') %>% 
        filter(type == 'TRADE_ACCEPT') %>% 
        select('team_id' = teamId, 
               'scoring_period_id' = scoringPeriodId) %>% 
        mutate('matchup_id' = week)
      
      
    })
  
  return(df)
}

get_trade_players <- function(df_trades) {
  df_players <- NULL
  for(i in 1:nrow(df_trades)) {
    scoring_period_accept <- df_trades$scoring_period_id[i]
    df_players <- 
      df_players %>% 
      bind_rows(
        df_daily %>% 
          filter(scoring_period_id >= scoring_period_accept,
                 scoring_period_id <= scoring_period_accept + 2) %>% 
          select(player, player_id, team_id, scoring_period_id)  %>% 
          group_by(player, player_id) %>% 
          summarise('n_app' = n(),
                    'n_team' = n_distinct(team_id),
                    'team_from' = first(team_id),
                    'team_to' = last(team_id), 
                    'scoring_period_id' = scoring_period_id[which(team_id != lag(team_id))]) %>% 
          filter(n_team > 1) %>% 
          filter(n_app == max(n_app)) %>% 
          filter(team_to == df_trades$team_id[i] | team_from == df_trades$team_id[i]) %>% 
          select(player, player_id, team_from, team_to, scoring_period_id) %>% 
          mutate('trade_id' = i)
      )
  }
  return(ungroup(df_players))
}
