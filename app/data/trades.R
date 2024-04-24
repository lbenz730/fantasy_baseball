library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(here)
library(furrr)

source(here('helpers.R'))

get_trades <- function(week, season_ = 2024, proposed = F) {
  df_start <- 
    read_csv(here('data/df_start.csv')) %>% 
    filter(season == season_)
  
  start <- df_start$start_period[week]
  end <- df_start$end_period[week]
  
  df <- 
    map_dfr(start:end, ~{
      w <- 
        robust_scrape(glue('https://lm-api-reads.fantasy.espn.com/apis/v3/games/flb/seasons/{season_}/segments/0/leagues/49106?scoringPeriodId={.x}&view=mTransactions2')) %>% 
        pluck('transactions') 
      
      if(!is.null(w)) {
        if(is.data.frame(w)) {
          w <- filter(w, (grepl('TRADE', type) & proposed) | (!proposed & type == 'TRADE_ACCEPT'))
        }
      }
      
      w
      
    })
  
  if(nrow(df) == 0) {
    df <- 
      tibble('team_id' = NA_real_,
             'scoring_period_id' = NA_real_,
             'matchup_id' = week) %>% 
      head(0)
    
    return(df)
  }
  
  if('relatedTransactionId' %in% names(df)) {
    df <- 
      df %>% 
      filter(!duplicated(relatedTransactionId)) 
  }
  
  if(!proposed) {
    df <- 
      df %>% 
      select('team_id' = teamId,
             'scoring_period_id' = scoringPeriodId) %>%
      mutate('matchup_id' = week)
  }
  
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
          reframe('n_app' = n(),
                  'n_team' = n_distinct(team_id),
                  'team_from' = first(team_id),
                  'team_to' = last(team_id), 
                  'scoring_period_id' = scoring_period_id[which(team_id != lag(team_id))]) %>% 
          filter(n_team > 1) %>% 
          filter(n_app == max(n_app)) %>% 
          filter( (team_to == df_trades$team_id[i] | team_from == df_trades$team_id[i])) %>% 
          select(player, player_id, team_from, team_to, scoring_period_id) %>% 
          group_by('teams_involved' = paste(pmin(team_to, team_from), pmax(team_to, team_from))) %>% 
          ungroup()
      ) %>% 
      filter(!duplicated(paste(player, player_id, team_from, team_to, scoring_period_id))) %>% 
      group_by(scoring_period_id, teams_involved) %>% 
      mutate('trade_id' = cur_group_id()) %>% 
      arrange(trade_id) %>% 
      ungroup()
  }
  
  return(ungroup(df_players))
}
