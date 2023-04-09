library(tidyverse)
library(here)
library(furrr)
library(glue)
source(here('helpers.R'))
plan(multisession(workers = min(parallel::detectCores(), 12)))
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2000)

get_trans_log <- function(season, trades = T) {
  df_daily <- read_csv(glue('data/stats/{season}/daily_stats_{season}.csv'))
  if(trades) {
    df_trades <- read_csv(glue('data/stats/{season}/traded_players_{season}.csv'))
  }
  
  trans_log <- 
    df_daily %>% 
    group_by(player, player_id) %>% 
    mutate('rostered_prev' = lag(scoring_period_id, 1) == scoring_period_id - 1) %>% 
    mutate('rostered_prev' = ifelse(is.na(rostered_prev), scoring_period_id == 1, rostered_prev)) %>% 
    mutate('free_agent_add' = !rostered_prev,
           'rp_eligible' = grepl('15', eligible_slots)) %>% 
    mutate('stint' = cumsum(free_agent_add)) %>% 
    group_by(player, player_id, team_id, stint) %>% 
    summarise('start' = min(scoring_period_id),
              'end' = max(scoring_period_id),
              'rp_eligible' = rp_eligible[scoring_period_id == min(scoring_period_id)]) %>% 
    ungroup() %>% 
    group_by(player, player_id) %>% 
    arrange(start)
  
  if(nrow(df_trades) > 0) {
    trans_log <- 
      trans_log %>% 
      left_join(df_trades, by = c('player_id', 'player', 'start' = 'scoring_period_id', 'team_id' = 'team_to')) %>% 
      mutate('transaction_type' = case_when(stint == 0 & start == 1 ~ 'Draft',
                                            !is.na(trade_id) ~ 'Trade',
                                            T ~ 'Free Agent')) %>% 
      ungroup()
  } else {
    trans_log <- 
      trans_log %>% 
      mutate('transaction_type' = case_when(stint == 0 & start == 1 ~ 'Draft',
                                            T ~ 'Free Agent'))
  }
  trans_log <- 
    trans_log %>% 
    mutate('nest_df' = 
             future_pmap(.l = list(player_id, start, end), ~{
               df_daily %>% 
                 filter(player_id == ..1,
                        scoring_period_id >= ..2,
                        scoring_period_id <= ..3) %>% 
                 summarise('n_points' = sum(points[in_lineup]),
                           'n_games' = sum(played[in_lineup]) + sum(relief[in_lineup] | start[in_lineup]),
                           'w_bat' = sum(played)/(sum(played) + sum(relief | start)),
                           'w_sp' = sum(start | relief_start)/(sum(played) + sum(relief | start)),
                           'w_rp' = 1 - w_bat - w_sp) %>% 
                 mutate('ppg' = n_points/n_games)
             })) %>% 
    unnest(cols = c(nest_df)) %>% 
    mutate('ppg' = replace(ppg, is.na(ppg), 0))
  write_csv(trans_log, glue('data/stats/{season}/transaction_log_{season}.csv'))
  
  return(trans_log)
}
